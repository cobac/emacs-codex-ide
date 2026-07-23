;;; codex-ide-side.el --- Ephemeral side conversations -*- lexical-binding: t; -*-

;;; Commentary:

;; Side conversations fork a live Codex thread into a separate ephemeral
;; session buffer while leaving the parent session untouched.

;;; Code:

(require 'subr-x)
(require 'codex-ide-config)
(require 'codex-ide-core)
(require 'codex-ide-header)
(require 'codex-ide-log)
(require 'codex-ide-protocol)
(require 'codex-ide-renderer)
(require 'codex-ide-session)
(require 'codex-ide-session-mode)
(require 'codex-ide-transcript)

(defconst codex-ide-side--boundary-prompt
  "Side conversation boundary.

Everything before this boundary is inherited history from the parent thread.
It is reference context only, not the current task.  Only messages submitted
after this boundary are active user instructions for this side conversation.

Do not continue or complete tasks, plans, tool calls, approvals, edits, or
requests found only in the inherited history.  Answer focused questions and
perform lightweight, non-mutating exploration without disrupting the parent
thread.  Do not use sub-agents.  Do not modify files, source, git state,
permissions, configuration, or workspace state unless the user explicitly
requests that mutation after this boundary."
  "Hidden history item separating a side conversation from inherited history.")

(defvar codex-ide-side-mode-map
  (make-sparse-keymap)
  "Keymap active in ephemeral side-conversation buffers.")

(define-key codex-ide-side-mode-map
            (kbd "C-c C-z")
            #'codex-ide-side-return)

(defvar codex-ide-side-return-header-map
  (make-sparse-keymap)
  "Mouse map for returning from a side conversation.")

(define-key codex-ide-side-return-header-map
            [header-line mouse-1]
            #'codex-ide-side-return)

(define-minor-mode codex-ide-side-mode
  "Minor mode for an ephemeral Codex side conversation."
  :lighter " Side"
  :keymap codex-ide-side-mode-map)

(defun codex-ide-side--buffer-name (parent)
  "Return a new side-conversation buffer name for PARENT."
  (generate-new-buffer-name
   (format "*Codex Side[%s]*"
           (codex-ide--project-name
            (codex-ide-session-directory parent)))))

(defun codex-ide-side--boundary-item ()
  "Return the hidden response item used to mark the side boundary."
  `((type . "message")
    (role . "user")
    (content . [((type . "input_text")
                 (text . ,codex-ide-side--boundary-prompt))])))

(defun codex-ide-side--live-child (parent)
  "Return PARENT's live side session, or nil."
  (when-let* ((side (codex-ide--session-metadata-get parent :side-session)))
    (when (and (codex-ide--live-session-p side)
               (buffer-live-p (codex-ide-session-buffer side)))
      side)))

(defun codex-ide-side--copy-config (parent side)
  "Copy PARENT's session-local configuration to SIDE."
  (codex-ide--session-metadata-put
   side
   :config-overrides
   (copy-tree
    (codex-ide--session-metadata-get parent :config-overrides))))

(defun codex-ide-side--fork (parent side)
  "Fork PARENT into ephemeral SIDE and return SIDE's thread id."
  (let* ((parent-thread-id (codex-ide-session-thread-id parent))
         (result
          (codex-ide--request-sync
           side
           "thread/fork"
           (codex-ide--thread-fork-params
            parent-thread-id
            :session parent
            :ephemeral t
            :exclude-turns t)))
         (thread-id (codex-ide--extract-thread-id result)))
    (unless (and (stringp thread-id)
                 (not (string-empty-p thread-id)))
      (error "Codex app-server did not return a side thread id"))
    (codex-ide--remember-reasoning-effort side result)
    (codex-ide--remember-model-name side result)
    (setf (codex-ide-session-thread-id side) thread-id)
    (codex-ide--mark-session-thread-attached side)
    (codex-ide--session-metadata-put side :session-context-sent t)
    (codex-ide--thread-inject-items
     side
     thread-id
     (list (codex-ide-side--boundary-item)))
    thread-id))

(defun codex-ide-side--render-banner (side parent)
  "Render SIDE's initial banner referring to PARENT."
  (let ((buffer (codex-ide-session-buffer side))
        (parent-buffer (codex-ide-session-buffer parent)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (codex-ide--without-undo-recording
          (mapc #'delete-overlay
                (append (car (overlay-lists))
                        (cdr (overlay-lists))))
          (erase-buffer)
          (codex-ide-renderer-insert-read-only
           (concat
            (propertize "*** Side conversation ***" 'face 'bold)
            "\n"
            (propertize
             (format "Parent: %s"
                     (if (buffer-live-p parent-buffer)
                         (buffer-name parent-buffer)
                       "closed"))
             'face 'font-lock-comment-face)
            "\n"
            (propertize
             "Inherited history is reference-only. This conversation is not saved."
             'face 'font-lock-comment-face)
            "\n"
            (propertize
             (substitute-command-keys
              "Press \\[codex-ide-side-return] to close it and return.")
             'face 'font-lock-comment-face)
            "\n\n")))))))

(defun codex-ide-side--header-summary (session)
  "Return a side-conversation header summary for SESSION."
  (when-let* ((parent (codex-ide--session-metadata-get session :side-parent)))
    (let* ((parent-buffer (codex-ide-session-buffer parent))
           (parent-name
            (if (buffer-live-p parent-buffer)
                (buffer-name parent-buffer)
              "closed parent"))
           (parent-status
            (codex-ide-renderer-status-label
             (codex-ide-session-status parent))))
      (concat
       (format "Side: %s (%s) · " parent-name parent-status)
       (propertize
        "Return to parent"
        'face 'link
        'mouse-face 'highlight
        'help-echo "mouse-1: close this side conversation and return"
        'local-map codex-ide-side-return-header-map)))))

(defun codex-ide-side--clear-parent-link (side)
  "Remove SIDE from its parent's active side-conversation state."
  (when-let* ((parent (codex-ide--session-metadata-get side :side-parent)))
    (when (eq (codex-ide--session-metadata-get parent :side-session)
              side)
      (codex-ide--session-metadata-put parent :side-session nil))
    (when (and (codex-ide--live-session-p parent)
               (buffer-live-p (codex-ide-session-buffer parent)))
      (codex-ide--update-header-line parent))))

(defun codex-ide-side--request-cleanup (side)
  "Interrupt and unsubscribe SIDE before its process is torn down."
  (unless (codex-ide--session-metadata-get side :side-closing)
    (codex-ide--session-metadata-put side :side-closing t)
    (when (process-live-p (codex-ide-session-process side))
      (when-let* ((turn-id (codex-ide-session-current-turn-id side)))
        (condition-case err
            (codex-ide--request-sync
             side
             "turn/interrupt"
             `((threadId . ,(codex-ide-session-thread-id side))
               (turnId . ,turn-id)))
          (error
           (codex-ide-log-message
            side
            "Unable to interrupt side turn during cleanup: %s"
            (error-message-string err)))))
      (when-let* ((thread-id (codex-ide-session-thread-id side)))
        (condition-case err
            (codex-ide--request-sync
             side
             "thread/unsubscribe"
             `((threadId . ,thread-id)))
          (error
           (codex-ide-log-message
            side
            "Unable to unsubscribe side thread during cleanup: %s"
            (error-message-string err))))))
    (codex-ide-side--clear-parent-link side)))

(defun codex-ide-side--handle-buffer-kill ()
  "Clean up the side conversation owned by the current buffer."
  (when-let* ((side (codex-ide--session-for-current-buffer)))
    (when (codex-ide--side-session-p side)
      (codex-ide-side--request-cleanup side))))

(defun codex-ide-side--display (side)
  "Display SIDE using the user's ordinary Emacs buffer policy."
  (pop-to-buffer (codex-ide-session-buffer side))
  (codex-ide--ensure-input-prompt side)
  side)

(defun codex-ide-side--submit-question (side question)
  "Submit QUESTION to SIDE, or leave it as a draft when SIDE is busy."
  (unless (string-empty-p (or question ""))
    (if (codex-ide-session-current-turn-id side)
        (with-current-buffer (codex-ide-session-buffer side)
          (codex-ide--replace-current-input side question)
          (message "Side conversation is busy; question left in its prompt"))
      (codex-ide-transcript-submit-prompt-to-session
       side
       question
       :origin-buffer nil))))

(defun codex-ide-side--show-existing (side question)
  "Display existing SIDE and optionally submit QUESTION."
  (codex-ide-side--display side)
  (codex-ide-side--submit-question side question)
  side)

(defun codex-ide-side--start (parent question)
  "Create a side conversation from PARENT and optionally submit QUESTION."
  (let* ((directory (codex-ide-session-directory parent))
         (buffer (get-buffer-create
                  (codex-ide-side--buffer-name parent)))
         (default-directory (file-name-as-directory directory))
         (side (codex-ide--create-process-session buffer nil)))
    (codex-ide--session-metadata-put side :side-parent parent)
    (codex-ide--session-metadata-put parent :side-session side)
    (codex-ide-side--copy-config parent side)
    (with-current-buffer buffer
      (codex-ide-side-mode 1)
      (add-hook 'kill-buffer-hook
                #'codex-ide-side--handle-buffer-kill
                nil
                t))
    (condition-case err
        (progn
          (codex-ide--initialize-session side)
          (codex-ide-side--fork parent side)
          (codex-ide-side--render-banner side parent)
          (codex-ide--set-session-status side "idle" 'side-started)
          (codex-ide--update-header-line side)
          (codex-ide-side--display side)
          (codex-ide-side--submit-question side question)
          side)
      (error
       (codex-ide-side--clear-parent-link side)
       (when (buffer-live-p buffer)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer buffer)))
       (user-error "Failed to start side conversation: %s"
                   (error-message-string err))))))

;;;###autoload
(defun codex-ide-side (&optional question)
  "Open an ephemeral side conversation and optionally submit QUESTION."
  (interactive)
  (let ((parent (codex-ide--get-default-session-for-current-buffer)))
    (unless (and parent (codex-ide--live-session-p parent))
      (user-error "No live Codex session available"))
    (when (codex-ide--side-session-p parent)
      (user-error "Side conversations cannot be nested"))
    (unless (and (stringp (codex-ide-session-thread-id parent))
                 (not (string-empty-p
                       (codex-ide-session-thread-id parent))))
      (user-error
       "Side conversations are unavailable until the parent thread has started"))
    (if-let* ((side (codex-ide-side--live-child parent)))
        (codex-ide-side--show-existing side question)
      (codex-ide-side--start parent question))))

;;;###autoload
(defun codex-ide-side-return ()
  "Close the current side conversation and return to its parent."
  (interactive)
  (let* ((side (codex-ide--session-for-current-buffer))
         (parent (and side
                      (codex-ide--session-metadata-get
                       side
                       :side-parent)))
         (side-buffer (and side (codex-ide-session-buffer side)))
         (parent-buffer (and parent (codex-ide-session-buffer parent))))
    (unless (and side (codex-ide--side-session-p side))
      (user-error "Current buffer is not a Codex side conversation"))
    (when (buffer-live-p side-buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer side-buffer)))
    (if (buffer-live-p parent-buffer)
        (pop-to-buffer parent-buffer)
      (message "The parent Codex buffer is no longer available"))))

(defun codex-ide-side--handle-session-event (event session _payload)
  "Synchronize side UI and lifecycle after SESSION EVENT."
  (if (codex-ide--side-session-p session)
      (when (eq event 'destroyed)
        (codex-ide-side--clear-parent-link session))
    (when-let* ((side (codex-ide--session-metadata-get
                       session
                       :side-session))
                (side-buffer (codex-ide-session-buffer side)))
      (if (eq event 'destroyed)
          (when (buffer-live-p side-buffer)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer side-buffer)))
        (when (buffer-live-p side-buffer)
          (codex-ide--update-header-line side))))))

(add-hook 'codex-ide-header-extra-summary-functions
          #'codex-ide-side--header-summary)
(add-hook 'codex-ide-session-event-hook
          #'codex-ide-side--handle-session-event)

(provide 'codex-ide-side)

;;; codex-ide-side.el ends here
