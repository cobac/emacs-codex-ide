;;; codex-ide-status-mode.el --- Project status overview for codex-ide -*- lexical-binding: t; -*-

;;; Commentary:

;; Project overview buffer for Codex buffers and stored threads.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'color)
(require 'seq)
(require 'subr-x)
(require 'codex-ide)
(require 'codex-ide-nav)
(require 'codex-ide-renderer)
(require 'codex-ide-section)
(require 'codex-ide-session-list)

(defvar codex-ide-display-buffer-pop-up-action)
(defvar codex-ide--display-buffer-other-window-pop-up-action)

;;;###autoload
(defcustom codex-ide-status-mode-transcript-preview-max-lines 40
  "Maximum number of transcript lines shown in expanded buffer sections."
  :type 'integer
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-status-mode-auto-refresh-delay 0.1
  "Idle delay in seconds before status buffers auto-refresh after session events."
  :type 'number
  :group 'codex-ide)

;;;###autoload
(defcustom codex-ide-status-mode-stripe-mix 0.12
  "How strongly status header striping blends toward the default foreground.

This controls the subtle alternating background used for every other session
header in `codex-ide-status-mode'.  The stripe color is computed by blending
the default background toward the default foreground by this fraction.

Smaller values produce a subtler stripe with lower contrast.  Larger values
produce a more visible stripe.  A value of 0 disables the effect entirely,
while 1 would fully replace the background with the foreground color."
  :type 'number
  :group 'codex-ide)

(defface codex-ide-status-expanded-content-face
  '((t :inherit default :background unspecified :extend t))
  "Face used for expanded buffer and thread content in status buffers."
  :group 'codex-ide)

(defface codex-ide-status-metadata-label-face
  '((t :inherit default :height 0.9 :weight light))
  "Face used for `* Label:' metadata prefixes in status buffers."
  :group 'codex-ide)

(defface codex-ide-status-striped-heading-face
  '((t :inherit default :background unspecified :extend t))
  "Face used for striped status section headings."
  :group 'codex-ide)

;; `defface' alone can leave an already-defined face carrying old attributes in a
;; live Emacs session, so explicitly clear the background on reload as well.
(face-spec-set 'codex-ide-status-expanded-content-face
               '((t :inherit default :background unspecified :extend t)))
(face-spec-set 'codex-ide-status-metadata-label-face
               '((t :inherit default :height 0.9 :weight light)))
(face-spec-set 'codex-ide-status-striped-heading-face
               '((t :inherit default :background unspecified :extend t)))

(defvar-local codex-ide-status-mode--directory nil
  "Project directory displayed by the current status buffer.")

(defvar-local codex-ide-status-mode--scope 'project
  "Session scope displayed by the current status buffer.")

(defvar-local codex-ide-status-mode--threads nil
  "Stored thread metadata loaded into the current status buffer.")

(defvar-local codex-ide-status-mode--next-cursor nil
  "Cursor for the next page of stored threads.")

(defvar-local codex-ide-status-mode--filter ""
  "Case-insensitive filter applied to loaded stored threads.")

(defvar-local codex-ide-status-mode--query-session nil
  "App-server session used to query stored thread metadata.")

(defvar-local codex-ide-status-mode--refresh-timer nil
  "Idle timer used to coalesce automatic status buffer refreshes.")

(defvar-local codex-ide-status-mode--event-listener nil
  "Function object registered on `codex-ide-session-event-hook' for this buffer.")

(defvar codex-ide-status-mode--theme-refresh-buffers nil
  "Live buffers currently using `codex-ide-status-mode' theme refresh hooks.")

(defvar codex-ide-status-mode--theme-refresh-timer nil
  "Timer used to coalesce deferred status face refreshes.")

(defvar codex-ide-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map codex-ide-section-mode-map)
    map)
  "Keymap for `codex-ide-status-mode'.")

(define-key codex-ide-status-mode-map (kbd "+") #'codex-ide)
(define-key codex-ide-status-mode-map (kbd "D") #'codex-ide-status-mode-delete-thing-at-point)
(define-key codex-ide-status-mode-map (kbd "K") #'codex-ide-status-mode-kill-buffer-at-point)
(define-key codex-ide-status-mode-map (kbd "l") #'codex-ide-status-mode-refresh)
(define-key codex-ide-status-mode-map (kbd "/") #'codex-ide-status-mode-filter)
(define-key codex-ide-status-mode-map (kbd "s") #'codex-ide-status-mode-filter)
(define-key codex-ide-status-mode-map (kbd "C-g") #'codex-ide-status-mode-quit)
(define-key codex-ide-status-mode-map (kbd "q") #'codex-ide-status-mode-quit)
(define-key codex-ide-status-mode-map (kbd "g") nil)
(define-key codex-ide-status-mode-map (kbd "g r") #'codex-ide-status-mode-refresh)
(define-key codex-ide-status-mode-map
            (kbd "RET")
            #'codex-ide-status-mode-display-session-at-point)
(define-key codex-ide-status-mode-map
            (kbd "M-<return>")
            #'codex-ide-status-mode-display-session-at-point-other-window)
(define-key codex-ide-status-mode-map
            (kbd "C-M-j")
            #'codex-ide-status-mode-display-session-at-point-other-window)
;; (define-key codex-ide-status-mode-map (kbd "TAB") #'codex-ide-status-mode-nav-forward)
;; (define-key codex-ide-status-mode-map (kbd "<backtab>") #'codex-ide-status-mode-nav-backward)

(defun codex-ide-status-mode--focal-points ()
  "Return focal points for the current status buffer."
  (append (codex-ide-nav-collect-sections)
          (codex-ide-nav-collect-buttons)))

;;;###autoload
(defun codex-ide-status-mode-nav-forward ()
  "Move point to the next focal point in a Codex status buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-status-mode)
    (user-error "Not in a Codex status buffer"))
  (codex-ide-nav-forward))

;;;###autoload
(defun codex-ide-status-mode-nav-backward ()
  "Move point to the previous focal point in a Codex status buffer."
  (interactive)
  (unless (derived-mode-p 'codex-ide-status-mode)
    (user-error "Not in a Codex status buffer"))
  (codex-ide-nav-backward))

(define-derived-mode codex-ide-status-mode codex-ide-section-mode "Codex-Status"
  "Major mode for the Codex project status buffer."
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  ;; Keep visual line motion so `next-line' follows the rendered section
  ;; layout when collapsed bodies hide their heading newline.
  (setq-local line-move-visual t)
  (when (bound-and-true-p visual-line-mode)
    (visual-line-mode -1))
  ;; (setq-local hl-line-face 'codex-ide-session-list-current-row-face)
  ;; (hl-line-mode 1)
  (setq-local codex-ide-nav-focal-point-functions
              '(codex-ide-status-mode--focal-points))
  (setq-local revert-buffer-function #'codex-ide-status-mode-refresh)
  (codex-ide-status-mode--teardown-auto-refresh)
  (codex-ide-status-mode--teardown-theme-refresh)
  (codex-ide-status-mode--setup-auto-refresh)
  (codex-ide-status-mode--setup-theme-refresh))

(defconst codex-ide-status-mode--session-events
  '(created destroyed thread-attached status-changed turn-started
            turn-completed approval-requested reset thread-deleted
            config-changed)
  "Session events that should trigger a status buffer refresh.")

(defun codex-ide-status-mode--cancel-refresh-timer ()
  "Cancel any pending automatic refresh timer for the current buffer."
  (when (timerp codex-ide-status-mode--refresh-timer)
    (cancel-timer codex-ide-status-mode--refresh-timer)
    (setq codex-ide-status-mode--refresh-timer nil)))

(defun codex-ide-status-mode--run-scheduled-refresh (buffer)
  "Refresh BUFFER if it is still a live Codex status buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq codex-ide-status-mode--refresh-timer nil)
      (when (and codex-ide-status-mode--directory
                 (derived-mode-p 'codex-ide-status-mode))
        (codex-ide-status-mode-refresh)))))

(defun codex-ide-status-mode--schedule-refresh ()
  "Schedule a coalesced refresh for the current status buffer."
  (unless (timerp codex-ide-status-mode--refresh-timer)
    (setq codex-ide-status-mode--refresh-timer
          (run-with-idle-timer
           codex-ide-status-mode-auto-refresh-delay
           nil
           #'codex-ide-status-mode--run-scheduled-refresh
           (current-buffer)))))

(defun codex-ide-status-mode--handle-session-event (event session _payload)
  "Schedule a refresh when EVENT for SESSION affects this status buffer."
  (when (and codex-ide-status-mode--directory
             (memq event codex-ide-status-mode--session-events)
             (or (eq codex-ide-status-mode--scope 'all)
                 (equal (codex-ide-session-directory session)
                        codex-ide-status-mode--directory)))
    (codex-ide-status-mode--schedule-refresh)))

(defun codex-ide-status-mode--setup-auto-refresh ()
  "Subscribe the current status buffer to Codex session events."
  (unless codex-ide-status-mode--event-listener
    (let ((buffer (current-buffer)))
      (setq codex-ide-status-mode--event-listener
            (lambda (event session payload)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (codex-ide-status-mode--handle-session-event
                   event session payload)))))))
  (add-hook 'codex-ide-session-event-hook codex-ide-status-mode--event-listener)
  (add-hook 'kill-buffer-hook #'codex-ide-status-mode--teardown-auto-refresh nil t))

(defun codex-ide-status-mode--teardown-auto-refresh ()
  "Remove session event subscriptions for the current status buffer."
  (codex-ide-status-mode--cancel-refresh-timer)
  (when codex-ide-status-mode--event-listener
    (remove-hook 'codex-ide-session-event-hook codex-ide-status-mode--event-listener)
    (setq codex-ide-status-mode--event-listener nil))
  (remove-hook 'kill-buffer-hook #'codex-ide-status-mode--teardown-auto-refresh t)
  (remove-hook 'change-major-mode-hook #'codex-ide-status-mode--teardown-auto-refresh t))

(defun codex-ide-status-mode--handle-theme-change (&rest _args)
  "Schedule status face refreshes after a theme change."
  (setq codex-ide-status-mode--theme-refresh-buffers
        (seq-filter #'buffer-live-p codex-ide-status-mode--theme-refresh-buffers))
  (when codex-ide-status-mode--theme-refresh-buffers
    (codex-ide-renderer-schedule-theme-refresh)
    (codex-ide-status-mode--schedule-theme-refresh)))

(defun codex-ide-status-mode--setup-theme-refresh ()
  "Subscribe the current status buffer to theme change events."
  (cl-pushnew (current-buffer) codex-ide-status-mode--theme-refresh-buffers)
  (codex-ide-renderer-schedule-theme-refresh)
  (codex-ide-status-mode--schedule-theme-refresh)
  (add-hook 'enable-theme-functions #'codex-ide-status-mode--handle-theme-change)
  (add-hook 'disable-theme-functions #'codex-ide-status-mode--handle-theme-change)
  (add-hook 'kill-buffer-hook #'codex-ide-status-mode--teardown-theme-refresh nil t)
  (add-hook 'change-major-mode-hook #'codex-ide-status-mode--teardown-theme-refresh nil t))

(defun codex-ide-status-mode--teardown-theme-refresh ()
  "Remove theme change subscriptions for the current status buffer."
  (setq codex-ide-status-mode--theme-refresh-buffers
        (delq (current-buffer)
              (seq-filter #'buffer-live-p codex-ide-status-mode--theme-refresh-buffers)))
  (unless codex-ide-status-mode--theme-refresh-buffers
    (when codex-ide-status-mode--theme-refresh-timer
      (cancel-timer codex-ide-status-mode--theme-refresh-timer)
      (setq codex-ide-status-mode--theme-refresh-timer nil))
    (remove-hook 'enable-theme-functions #'codex-ide-status-mode--handle-theme-change)
    (remove-hook 'disable-theme-functions #'codex-ide-status-mode--handle-theme-change))
  (remove-hook 'kill-buffer-hook #'codex-ide-status-mode--teardown-theme-refresh t)
  (remove-hook 'change-major-mode-hook #'codex-ide-status-mode--teardown-theme-refresh t))

(defun codex-ide-status-mode--section-identity (section)
  "Return a stable identity for SECTION across rerenders."
  (pcase (codex-ide-section-type section)
    ('buffers 'buffers)
    ('threads 'threads)
    ('buffer
     (when-let* ((session (codex-ide-section-value section))
                 (buffer (and (codex-ide-session-p session)
                              (codex-ide-session-buffer session))))
       (buffer-name buffer)))
    ('thread
     (let ((thread (codex-ide-section-value section)))
       (or (alist-get 'id thread)
           (alist-get 'name thread)
           (alist-get 'preview thread))))
    (_ (codex-ide-section-type section))))

(defun codex-ide-status-mode--all-sessions-p ()
  "Return non-nil when the current status buffer shows all sessions."
  (eq codex-ide-status-mode--scope 'all))

(defun codex-ide-status-mode--thread-directory (thread)
  "Return the working directory associated with THREAD."
  (if (codex-ide-status-mode--all-sessions-p)
      (alist-get 'cwd thread)
    codex-ide-status-mode--directory))

(defun codex-ide-status-mode--actionable-section-at-point ()
  "Return the actionable status section at point.
Only child `buffer' and `thread' sections support visit and delete actions."
  (let ((section (codex-ide-section-at-point)))
    (unless section
      (user-error "No status entry at point"))
    (unless (memq (codex-ide-section-type section) '(buffer thread))
      (user-error "No status entry at point"))
    section))

(defun codex-ide-status-mode--selected-actionable-sections ()
  "Return actionable sections at point or every unique one touched by the active region."
  (if (use-region-p)
      (let ((sections nil)
            (end (max (region-beginning) (1- (region-end)))))
        (save-excursion
          (goto-char (region-beginning))
          (beginning-of-line)
          (while (<= (point) end)
            (when-let* ((section (codex-ide-section-containing-point)))
              (when (and (memq (codex-ide-section-type section) '(buffer thread))
                         (not (memq section sections)))
                (push section sections)))
            (forward-line 1)))
        (or (nreverse sections)
            (user-error "No status entries in region")))
    (list (codex-ide-status-mode--actionable-section-at-point))))

(defun codex-ide-status-mode--visit-section (section)
  "Visit SECTION using the same underlying behavior as the session list modes."
  (pcase (codex-ide-section-type section)
    ('buffer
     (let ((session (codex-ide-section-value section)))
       (unless (and (codex-ide-session-p session)
                    (buffer-live-p (codex-ide-session-buffer session)))
         (user-error "Session buffer is no longer live"))
       (codex-ide--show-session-buffer session)))
    ('thread
     (let* ((thread (codex-ide-section-value section))
            (directory (codex-ide-status-mode--thread-directory thread)))
       (unless (and (stringp directory) (not (string-empty-p directory)))
         (user-error "Stored Codex session has no working directory"))
       (codex-ide--prepare-session-operations)
       (codex-ide--show-or-resume-thread (alist-get 'id thread) directory)))))

(defun codex-ide-status-mode--delete-buffer-session (session)
  "Delete SESSION's live buffer with list-mode-consistent confirmation."
  (let ((buffer (and (codex-ide-session-p session)
                     (codex-ide-session-buffer session))))
    (unless (buffer-live-p buffer)
      (user-error "Session buffer is no longer live"))
    (when (y-or-n-p
           (format "Kill Codex session buffer %s? " (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))
      (codex-ide-status-mode-refresh))))

(defun codex-ide-status-mode--delete-thread (thread)
  "Delete THREAD with list-mode-consistent confirmation and refresh."
  (codex-ide--prepare-session-operations)
  (let ((codex-home (abbreviate-file-name (codex-ide--codex-home))))
    (when (yes-or-no-p
           (format "Permanently remove 1 Codex thread from %s? " codex-home))
      (codex-ide-delete-session-thread (alist-get 'id thread) t)
      (codex-ide-status-mode-refresh))))

(defun codex-ide-status-mode--confirm-delete-sections (sections)
  "Return non-nil when the user confirms deleting SECTIONS."
  (let* ((count (length sections))
         (buffer-count (seq-count
                        (lambda (section)
                          (eq (codex-ide-section-type section) 'buffer))
                        sections))
         (thread-count (- count buffer-count))
         (codex-home (abbreviate-file-name (codex-ide--codex-home)))
         (prompt nil)
         (confirm-function nil))
    (cond
     ((= buffer-count count)
      (setq prompt
            (if (= count 1)
                (format "Kill Codex session buffer %s? "
                        (buffer-name
                         (codex-ide-session-buffer
                          (codex-ide-section-value (car sections)))))
              (format "Kill %d Codex session buffers? " count))
            confirm-function #'y-or-n-p))
     ((= thread-count count)
      (setq prompt
            (if (= count 1)
                (format "Permanently remove 1 Codex thread from %s? " codex-home)
              (format "Permanently remove %d Codex threads from %s? "
                      count
                      codex-home))
            confirm-function #'yes-or-no-p))
     (t
      (setq prompt (format "Delete %d Codex status entries? " count)
            confirm-function #'y-or-n-p)))
    (funcall confirm-function prompt)))

(defun codex-ide-status-mode--delete-sections (sections)
  "Delete SECTIONS after one confirmation and refresh once."
  (when (codex-ide-status-mode--confirm-delete-sections sections)
    (when (seq-some (lambda (section)
                      (eq (codex-ide-section-type section) 'thread))
                    sections)
      (codex-ide--prepare-session-operations))
    (dolist (section sections)
      (pcase (codex-ide-section-type section)
        ('buffer
         (let* ((session (codex-ide-section-value section))
                (buffer (and (codex-ide-session-p session)
                             (codex-ide-session-buffer session))))
           (unless (buffer-live-p buffer)
             (user-error "Session buffer is no longer live"))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buffer))))
        ('thread
         (codex-ide-delete-session-thread
          (alist-get 'id (codex-ide-section-value section))
          t))))
    (codex-ide-status-mode-refresh)))

(defun codex-ide-status-mode--section-buffer (section)
  "Return SECTION's live session buffer, or nil when it has none."
  (when-let* ((session (pcase (codex-ide-section-type section)
                         ('buffer
                          (codex-ide-section-value section))
                         ('thread
                          (codex-ide-status-mode--thread-session
                           (codex-ide-section-value section)
                           codex-ide-status-mode--directory))))
              (buffer (and (codex-ide-session-p session)
                           (codex-ide-session-buffer session))))
    (and (buffer-live-p buffer) buffer)))

(defun codex-ide-status-mode--confirm-kill-buffers (buffers)
  "Return non-nil when the user confirms killing BUFFERS."
  (y-or-n-p
   (if (= (length buffers) 1)
       (format "Kill Codex session buffer %s? "
               (buffer-name (car buffers)))
     (format "Kill %d Codex session buffers? " (length buffers)))))

(defun codex-ide-status-mode--kill-buffers (buffers)
  "Kill live session BUFFERS after one confirmation and refresh once."
  (let ((live-buffers nil))
    (dolist (buffer buffers)
      (when (and (buffer-live-p buffer)
                 (not (memq buffer live-buffers)))
        (push buffer live-buffers)))
    (setq live-buffers (nreverse live-buffers))
    (when (and live-buffers
               (codex-ide-status-mode--confirm-kill-buffers live-buffers))
      (dolist (buffer live-buffers)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer)))
      (codex-ide-status-mode-refresh))))

(defun codex-ide-status-mode-display-session-at-point ()
  "Display the session for the actionable status entry at point."
  (interactive)
  (codex-ide-status-mode--visit-section
   (codex-ide-status-mode--actionable-section-at-point)))

(defun codex-ide-status-mode-display-session-at-point-other-window ()
  "Display the session for the actionable status entry at point in another window."
  (interactive)
  (let ((codex-ide-display-buffer-pop-up-action
         codex-ide--display-buffer-other-window-pop-up-action))
    (codex-ide-status-mode-display-session-at-point)))

(defun codex-ide-status-mode-delete-thing-at-point ()
  "Delete the actionable status entry at point or every entry in the active region."
  (interactive)
  (codex-ide-status-mode--delete-sections
   (codex-ide-status-mode--selected-actionable-sections)))

(defun codex-ide-status-mode-kill-buffer-at-point ()
  "Kill the session buffer at point or every live session buffer in the active region."
  (interactive)
  (let* ((sections (codex-ide-status-mode--selected-actionable-sections))
         (buffers (delq nil (mapcar #'codex-ide-status-mode--section-buffer sections))))
    (when (and (not (use-region-p))
               (null buffers))
      (user-error "No buffer for this session."))
    (codex-ide-status-mode--kill-buffers buffers)))

(defun codex-ide-status-mode--status-face (status)
  "Return the face used for STATUS."
  (or (codex-ide-renderer-status-face status) 'default))

(defun codex-ide-status-mode--header-line (directory count)
  "Return header-line text for DIRECTORY with session COUNT."
  (format "%s | %d %s"
          (if (codex-ide-status-mode--all-sessions-p)
              "All sessions"
            (format "Project: %s" (codex-ide--project-name directory)))
          count
          (if (= count 1) "session" "sessions")))

(defun codex-ide-status-mode--striped-heading-background ()
  "Return a theme-aware background color for striped headings."
  (let ((background (face-background 'default nil t))
        (foreground (face-foreground 'default nil t)))
    (when (and (stringp background)
               (stringp foreground)
               (not (member background '("unspecified" "unspecified-bg")))
               (not (member foreground '("unspecified" "unspecified-fg")))
               (color-defined-p background)
               (color-defined-p foreground))
      (let ((background-rgb (color-name-to-rgb background))
            (foreground-rgb (color-name-to-rgb foreground))
            (mix (max 0.0 (min 1.0 codex-ide-status-mode-stripe-mix))))
        (when (and background-rgb foreground-rgb)
          (apply #'color-rgb-to-hex
                 (append
                  (cl-mapcar
                   (lambda (background-channel foreground-channel)
                     (+ (* background-channel (- 1.0 mix))
                        (* foreground-channel mix)))
                   background-rgb
                   foreground-rgb)
                  '(2))))))))

(defun codex-ide-status-mode--refresh-striped-heading-face ()
  "Refresh the striped heading face for the current theme."
  (let ((background (codex-ide-status-mode--striped-heading-background)))
    (set-face-attribute
     'codex-ide-status-striped-heading-face
     nil
     :inherit 'default
     :background (or background 'unspecified)
     :extend t)))

(defun codex-ide-status-mode--run-scheduled-theme-refresh ()
  "Run a deferred status face refresh."
  (setq codex-ide-status-mode--theme-refresh-timer nil)
  (when (seq-some #'buffer-live-p codex-ide-status-mode--theme-refresh-buffers)
    (codex-ide-status-mode--refresh-striped-heading-face)))

(defun codex-ide-status-mode--schedule-theme-refresh ()
  "Schedule a coalesced refresh of theme-sensitive status faces."
  (unless codex-ide-status-mode--theme-refresh-timer
    (setq codex-ide-status-mode--theme-refresh-timer
          (run-at-time 0 nil #'codex-ide-status-mode--run-scheduled-theme-refresh))))

(defun codex-ide-status-mode--apply-heading-stripe (section)
  "Apply stripe styling to SECTION's heading."
  (add-face-text-property
   (codex-ide-section-heading-start section)
   (codex-ide-section-heading-end section)
   'codex-ide-status-striped-heading-face
   'append))

(defun codex-ide-status-mode--prompt-text-at (start &optional limit)
  "Return prompt text for the prompt beginning at START before LIMIT."
  (let* ((end (next-single-char-property-change
               start
               'face
               nil
               (or limit (point-max))))
         (text (string-remove-prefix
                "> "
                (buffer-substring-no-properties start end))))
    (string-trim text)))

(defun codex-ide-status-mode--prompt-start-p (&optional pos)
  "Return non-nil when POS is the start of a prompt line."
  (get-text-property (or pos (point)) codex-ide-prompt-start-property))

(defun codex-ide-status-mode--last-submitted-prompt-text (session)
  "Return the last submitted non-empty prompt text from SESSION."
  (let ((search-end (or (and (codex-ide-session-input-prompt-start-marker session)
                             (marker-position
                              (codex-ide-session-input-prompt-start-marker session)))
                        (point-max))))
    (plist-get (codex-ide-status-mode--last-prompt-data-before session search-end)
               :text)))

(defun codex-ide-status-mode--first-prompt-data (session)
  "Return plist describing the first non-empty submitted prompt in SESSION.
The plist contains `:text', `:start', and `:end'."
  (when-let* ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let (found)
            (goto-char (point-min))
            (while (and (not found)
                        (re-search-forward "^> " nil t))
              (let ((start (match-beginning 0)))
                (when (codex-ide-status-mode--prompt-start-p start)
                  (let ((text (codex-ide-status-mode--prompt-text-at start)))
                    (unless (string-empty-p text)
                      (setq found
                            (list :text text
                                  :start start
                                  :end (next-single-char-property-change
                                        start 'face nil (point-max)))))))))
            found))))))

(defun codex-ide-status-mode--first-submitted-prompt-text (session)
  "Return the first submitted non-empty prompt text from SESSION."
  (plist-get (codex-ide-status-mode--first-prompt-data session) :text))

(defun codex-ide-status-mode--submitted-prompt-count (session)
  "Return the number of non-empty submitted prompts in SESSION."
  (let ((count 0)
        (position most-positive-fixnum)
        prompt-data)
    (while (setq prompt-data
                 (codex-ide-status-mode--last-prompt-data-before session position))
      (setq count (1+ count)
            position (plist-get prompt-data :start)))
    count))

(defun codex-ide-status-mode--active-prompt-data (session)
  "Return plist describing SESSION's current non-empty editable prompt."
  (when-let* ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when-let* ((prompt-start (and (codex-ide-session-input-prompt-start-marker session)
                                       (marker-position
					(codex-ide-session-input-prompt-start-marker session))))
                    (input-start (and (codex-ide-session-input-start-marker session)
                                      (marker-position
                                       (codex-ide-session-input-start-marker session)))))
          (let ((text (codex-ide--current-input session)))
            (unless (string-empty-p text)
              (list :text text
                    :start prompt-start
                    :end (or (and (fboundp 'codex-ide--input-end-position)
                                  (codex-ide--input-end-position session))
                             input-start)))))))))

(defun codex-ide-status-mode--last-prompt-data-before (session position)
  "Return plist describing the last non-empty prompt in SESSION before POSITION.
The plist contains `:text', `:start', and `:end'."
  (when-let* ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let ((limit (max (point-min)
                            (min (point-max) position)))
                candidate
                candidate-start
                candidate-end)
            (goto-char limit)
            (while (and (not candidate-start)
                        (re-search-backward "^> " nil t))
              (let ((start (point)))
                (when (and (< start limit)
                           (codex-ide-status-mode--prompt-start-p start))
                  (let ((text (codex-ide-status-mode--prompt-text-at start limit)))
                    (unless (string-empty-p text)
                      (setq candidate text
                            candidate-start start
                            candidate-end (next-single-char-property-change
                                           start 'face nil limit)))))))
            (when candidate-start
              (list :text candidate
                    :start candidate-start
                    :end candidate-end))))))))

(defun codex-ide-status-mode--last-prompt-data (session)
  "Return plist describing the last non-empty prompt in SESSION.
The plist contains `:text', `:start', and `:end'."
  (or (codex-ide-status-mode--active-prompt-data session)
      (codex-ide-status-mode--last-prompt-data-before session (point-max))))

(defun codex-ide-status-mode--last-submitted-prompt-data (session)
  "Return plist describing SESSION's last submitted prompt."
  (when-let* ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((search-end (if (codex-ide--input-prompt-active-p session)
                              (or (and (codex-ide-session-input-prompt-start-marker session)
                                       (marker-position
                                        (codex-ide-session-input-prompt-start-marker session)))
                                  (point-max))
                            (point-max))))
          (codex-ide-status-mode--last-prompt-data-before session search-end))))))

(defun codex-ide-status-mode--preview-line (value)
  "Return a one-line preview for VALUE."
  (let ((preview (replace-regexp-in-string
                  "[\n\r]+"
                  "↵"
                  (codex-ide--thread-choice-preview (or value "")))))
    (if (string-empty-p preview)
        "Untitled"
      preview)))

(defun codex-ide-status-mode--plain-text (value)
  "Return VALUE without text properties."
  (when value
    (substring-no-properties value)))

(defun codex-ide-status-mode--format-heading-status (label status)
  "Return LABEL styled for STATUS."
  (propertize label
              'face
              (codex-ide-status-mode--status-face status)))

(defun codex-ide-status-mode--format-heading-updated (text)
  "Return styled updated display TEXT for section headings."
  (propertize (or text "")
              'face
              'shadow))

(defun codex-ide-status-mode--format-heading-preview (preview)
  "Return PREVIEW styled for section headings."
  preview)

(defun codex-ide-status-mode--thread-name (thread)
  "Return THREAD's non-empty user-facing name, or nil."
  (when-let* ((name (alist-get 'name thread))
              ((stringp name))
              (name (string-trim name))
              ((not (string-empty-p name))))
    name))

(defun codex-ide-status-mode--full-thread-preview (thread)
  "Return THREAD's complete human-authored preview."
  (let ((preview
         (codex-ide--thread-choice-preview
          (or (alist-get 'preview thread) ""))))
    (if (string-empty-p preview) "Untitled" preview)))

(defun codex-ide-status-mode--filter-match-p (thread)
  "Return non-nil when THREAD matches the active status filter."
  (or (string-empty-p codex-ide-status-mode--filter)
      (let* ((directory (codex-ide-status-mode--thread-directory thread))
             (session (codex-ide-status-mode--thread-session thread directory))
             (search-text
              (downcase
               (string-join
                (list (codex-ide-status-mode--full-thread-preview thread)
                      (or (codex-ide-status-mode--thread-name thread) "")
                      (or (alist-get 'cwd thread) "")
                      (or (alist-get 'id thread) "")
                      (or (and session
                               (codex-ide-status-mode--last-submitted-prompt-text
                                session))
                          "")
                      (or (and session
                               (codex-ide-status-mode--buffer-transcript-slice
                                session))
                          ""))
                "\n"))))
        (string-match-p
         (regexp-quote (downcase codex-ide-status-mode--filter))
         search-text))))

(defun codex-ide-status-mode--visible-threads ()
  "Return loaded threads matching the current status filter."
  (seq-filter #'codex-ide-status-mode--filter-match-p
              codex-ide-status-mode--threads))

(defun codex-ide-status-mode--pad-heading-part (text width)
  "Return TEXT padded with trailing spaces to WIDTH."
  (concat text (make-string (max 0 (- width (string-width text))) ?\s)))

(defun codex-ide-status-mode--heading-layout (threads directory)
  "Return heading layout widths for THREADS using fallback DIRECTORY."
  (let ((status-width 0)
        (updated-width 0))
    (dolist (thread threads)
      (let* ((thread-directory
              (if (codex-ide-status-mode--all-sessions-p)
                  (codex-ide-status-mode--thread-directory thread)
                directory))
             (session (codex-ide-status-mode--thread-session
                       thread thread-directory))
             (status (if session
                         (codex-ide-session-status session)
                       "stored"))
             (label (codex-ide-renderer-status-label status))
             (updated (or (codex-ide-human-time-ago (alist-get 'updatedAt thread)) "")))
        (setq status-width (max status-width (string-width label))
              updated-width (max updated-width (string-width updated)))))
    (list :status-width status-width
          :updated-width updated-width)))

(defun codex-ide-status-mode--thread-updated-at-time (thread)
  "Return THREAD's `updatedAt' value as an Emacs time."
  (let ((updated-at (alist-get 'updatedAt thread)))
    (cond
     ((numberp updated-at) (seconds-to-time updated-at))
     ((stringp updated-at) (or (ignore-errors (date-to-time updated-at))
                               (seconds-to-time 0)))
     (t (seconds-to-time 0)))))

(defun codex-ide-status-mode--sort-threads-by-updated (threads)
  "Return THREADS sorted by most recently updated first."
  (sort (copy-sequence threads)
        (lambda (left right)
          (time-less-p
           (codex-ide-status-mode--thread-updated-at-time right)
           (codex-ide-status-mode--thread-updated-at-time left)))))

(defun codex-ide-status-mode--last-agent-response-range (session)
  "Return the start and end positions of SESSION's last agent response."
  (when-let* ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let* ((search-end (if (codex-ide--input-prompt-active-p session)
                                 (or (and (codex-ide-session-input-prompt-start-marker session)
                                          (marker-position
                                           (codex-ide-session-input-prompt-start-marker session)))
                                     (point-max))
                               (point-max)))
                 (pos search-end)
                 response-start
                 response-end)
            (while (and (> pos (point-min)) (not response-end))
              (setq pos (previous-single-char-property-change
                         pos
                         codex-ide-agent-item-type-property
                         nil
                         (point-min)))
              (when (and (< pos search-end)
                         (get-text-property pos codex-ide-agent-item-type-property))
                (setq response-end
                      (next-single-char-property-change
                       pos
                       codex-ide-agent-item-type-property
                       nil
                       search-end))
                (setq response-start pos)
                (while (and (> response-start (point-min))
                            (get-text-property
                             (1- response-start)
                             codex-ide-agent-item-type-property))
                  (setq response-start
                        (previous-single-char-property-change
                         response-start
                         codex-ide-agent-item-type-property
                         nil
                         (point-min))))))
            (when (and response-start response-end)
              (cons response-start response-end))))))))

(defun codex-ide-status-mode--copy-buffer-region-for-status (session start end)
  "Return SESSION buffer text from START to END."
  (ignore session)
  (buffer-substring start end))

(defun codex-ide-status-mode--insert-prefixed-lines
    (text &optional content-face prefix prefix-face)
  "Insert TEXT with PREFIX on each line.
When CONTENT-FACE is non-nil, apply it to each inserted line body.
PREFIX defaults to a dimmed `└ '.  PREFIX-FACE defaults to `shadow'."
  (when (stringp text)
    (let ((target (current-buffer))
          (prefix (or prefix "└ "))
          (prefix-face (or prefix-face 'shadow)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((line-start (point))
                (line-end (line-end-position))
                (has-newline (< (line-end-position) (point-max)))
                line-text)
            (setq line-text (buffer-substring line-start line-end))
            (with-current-buffer target
              (let (content-start content-end)
                (insert (propertize prefix 'face prefix-face))
                (setq content-start (point))
                (insert line-text)
                (setq content-end (point))
                (when content-face
                  (add-text-properties content-start content-end
                                       (list 'face content-face)))
                (when has-newline
                  (insert "\n"))))
            (forward-line 1)))))))

(defun codex-ide-status-mode--apply-expanded-content-face (start end)
  "Apply the expanded-content face between START and END."
  (when (< start end)
    (add-face-text-property
     start
     end
     'codex-ide-status-expanded-content-face
     'append)))

(defun codex-ide-status-mode--skip-transcript-leading-junk (start end)
  "Return the first content position between START and END.
Skip separator borders and leading whitespace."
  (save-excursion
    (goto-char start)
    (let ((separator (codex-ide-renderer-output-separator-string))
          (separator-regexp (regexp-quote (codex-ide-renderer-output-separator-string)))
          done)
      (while (and (< (point) end) (not done))
        (let ((before (point)))
          (skip-chars-forward " \t\n" end)
          (cond
           ((and (<= (+ (point) (length separator)) end)
                 (looking-at-p separator-regexp))
            (forward-char (length separator)))
           ((= (point) before)
            (setq done t)))))
      (min (point) end))))

(defun codex-ide-status-mode--trim-transcript-end (start end)
  "Return the last content position between START and END.
Trim trailing separator borders and whitespace."
  (save-excursion
    (goto-char end)
    (let ((separator (codex-ide-renderer-output-separator-string)))
      (while (and (> (point) start)
                  (progn
                    (skip-chars-backward " \t\n" start)
                    (if (and (>= (- (point) (length separator)) start)
                             (save-excursion
                               (goto-char (- (point) (length separator)))
                               (looking-at-p (regexp-quote separator))))
                        (progn
                          (goto-char (- (point) (length separator)))
                          t)
                      nil))))
      (skip-chars-backward " \t\n" start)
      (max start (point)))))

(defun codex-ide-status-mode--transcript-preview-range (start end)
  "Return the transcript preview range between START and END.
The preview shows the last
`codex-ide-status-mode-transcript-preview-max-lines' lines, expanded to begin
at the start of the containing separator-delimited block."
  (save-excursion
    (let ((trimmed-end (codex-ide-status-mode--trim-transcript-end start end)))
      (when (< start trimmed-end)
        (goto-char trimmed-end)
        (forward-line (- (max 0 codex-ide-status-mode-transcript-preview-max-lines)))
        (let* ((separator (codex-ide-renderer-output-separator-string))
               (line-start (max start (line-beginning-position)))
               (block-start
                (progn
                  (goto-char line-start)
                  (if-let* ((separator-start (search-backward separator start t)))
                      (+ separator-start (length separator))
                    start)))
               (content-start
                (codex-ide-status-mode--skip-transcript-leading-junk
                 block-start trimmed-end)))
          (when (< content-start trimmed-end)
            (cons content-start trimmed-end)))))))

(defun codex-ide-status-mode--last-output-block-range (start end)
  "Return the last separator-delimited output block between START and END."
  (save-excursion
    (let* ((trimmed-end (codex-ide-status-mode--trim-transcript-end start end))
           (separator (codex-ide-renderer-output-separator-string)))
      (when (< start trimmed-end)
        (goto-char trimmed-end)
        (let* ((block-start
                (if-let* ((separator-start
                           (search-backward separator start t)))
                    (+ separator-start (length separator))
                  start))
               (content-start
                (codex-ide-status-mode--skip-transcript-leading-junk
                 block-start trimmed-end)))
          (when (< content-start trimmed-end)
            (cons content-start trimmed-end)))))))

(defun codex-ide-status-mode--current-turn-transcript-range (session)
  "Return the visible transcript range for SESSION's in-progress turn."
  (when-let* ((prompt-data (codex-ide-status-mode--last-submitted-prompt-data session))
              (buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (let* ((turn-start (plist-get prompt-data :end))
                 (turn-end (if (codex-ide--input-prompt-active-p session)
                               (or (and (codex-ide-session-input-prompt-start-marker session)
                                        (marker-position
                                         (codex-ide-session-input-prompt-start-marker session)))
                                   (point-max))
                             (point-max))))
            (when-let* ((block-range
                         (codex-ide-status-mode--last-output-block-range
                          turn-start
                          turn-end)))
              (let ((start (car block-range))
                    (end (cdr block-range)))
                (goto-char start)
                (while (and (< (point) end)
                            (eq (char-after) ?\n))
                  (forward-char 1))
                (setq start (point))
                (goto-char end)
                (while (and (> (point) start)
                            (memq (char-before) '(?\n ?\s ?\t)))
                  (backward-char 1))
                (setq end (point))
                (when (< start end)
                  (cons start end))))))))))

(defun codex-ide-status-mode--buffer-transcript-slice (session)
  "Return the last relevant transcript slice for SESSION.
This includes the in-progress turn transcript or the last completed reply block.
Return nil when there is no agent reply."
  (when-let* ((buffer (codex-ide-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (and (codex-ide--input-prompt-active-p session)
                 (string= (codex-ide-session-status session) "idle"))
            (when-let* ((response-range
                         (codex-ide-status-mode--last-agent-response-range session))
                        (block-range
                         (codex-ide-status-mode--transcript-preview-range
                          (car response-range)
                          (cdr response-range))))
              (codex-ide-status-mode--copy-buffer-region-for-status
               session
               (car block-range)
               (cdr block-range)))
          (when-let* ((turn-range
                       (codex-ide-status-mode--current-turn-transcript-range session)))
            (codex-ide-status-mode--copy-buffer-region-for-status
             session
             (car turn-range)
             (cdr turn-range))))))))

(defun codex-ide-status-mode--project-sessions (directory)
  "Return live session buffers for DIRECTORY."
  (sort
   (seq-filter
    (lambda (session)
      (equal (codex-ide-session-directory session) directory))
    (codex-ide--session-buffer-sessions))
   (lambda (left right)
     (string-lessp (buffer-name (codex-ide-session-buffer left))
                   (buffer-name (codex-ide-session-buffer right))))))

(defun codex-ide-status-mode--thread-status (thread directory)
  "Return the display status for THREAD in DIRECTORY."
  (if-let* ((session (codex-ide--session-for-thread-id
                      (alist-get 'id thread)
                      directory)))
      (codex-ide-session-status session)
    "stored"))

(defun codex-ide-status-mode--thread-session (thread directory)
  "Return the live session linked to THREAD in DIRECTORY, if any."
  (when directory
    (codex-ide--session-for-thread-id (alist-get 'id thread) directory)))

(defun codex-ide-status-mode--insert-thread-metadata-line (label value)
  "Insert a thread metadata line with LABEL and VALUE."
  (let ((start (point)))
    (insert (propertize (format "* %s:" label)
                        'face
                        'codex-ide-status-metadata-label-face))
    (insert " ")
    (let ((value-start (point)))
      (insert (or value ""))
      (add-text-properties
       value-start
       (point)
       (list 'face
             (pcase label
               ((or "Thread ID" "Created" "Updated") 'font-lock-string-face)
               ("Buffer" 'warning)
               ((or "Last Prompt" "Last Response") 'font-lock-doc-face)
               (_ 'default)))))
    (insert "\n")
    (codex-ide-status-mode--apply-expanded-content-face start (point))))

(defun codex-ide-status-mode--insert-buffer-metadata-line (session)
  "Insert a clickable buffer metadata line for SESSION."
  (let ((start (point))
        (buffer (codex-ide-session-buffer session)))
    (insert (propertize "* Buffer:"
                        'face
                        'codex-ide-status-metadata-label-face))
    (insert " ")
    (insert-text-button
     (buffer-name buffer)
     'face 'button
     'follow-link t
     'help-echo "Open session buffer"
     'mouse-face 'highlight
     'keymap (codex-ide-nav-button-keymap)
     'action (lambda (_button)
               (when (buffer-live-p buffer)
                 (codex-ide--show-session-buffer session))))
    (insert "\n")
    (codex-ide-status-mode--apply-expanded-content-face start (point))))

(defun codex-ide-status-mode--thread-preview-body (full-preview)
  "Return FULL-PREVIEW normalized for status display."
  (or (codex-ide--thread-read-display-user-text full-preview)
      ""))

(defun codex-ide-status-mode--insert-thread-preview-body (full-preview)
  "Insert FULL-PREVIEW as plain status preview text."
  (ignore full-preview))

(defun codex-ide-status-mode--insert-buffer-section (session)
  "Insert a child section for SESSION."
  (let* ((status (codex-ide-session-status session))
         (label (codex-ide-renderer-status-label status))
         (first-prompt (or (codex-ide-status-mode--first-submitted-prompt-text session) ""))
         (prompt-count (codex-ide-status-mode--submitted-prompt-count session))
         (last-prompt (or (codex-ide-status-mode--last-submitted-prompt-text session) ""))
         (last-response (or (codex-ide-status-mode--buffer-transcript-slice session) ""))
         (preview (codex-ide-status-mode--preview-line first-prompt))
         (title (concat
                 (codex-ide-status-mode--format-heading-status label status)
                 "  "
                 (codex-ide-status-mode--format-heading-preview preview))))
    (codex-ide-section-insert
     'buffer session title
     (lambda (_section)
       (let ((start (point)))
         (codex-ide-status-mode--insert-buffer-metadata-line session)
         (codex-ide-status-mode--insert-thread-metadata-line
          "Number of Prompts"
          (number-to-string prompt-count))
         (codex-ide-status-mode--insert-thread-metadata-line
          "Last Prompt"
          (codex-ide-status-mode--preview-line last-prompt))
         (codex-ide-status-mode--insert-thread-metadata-line
          "Last Response"
          (codex-ide-status-mode--preview-line
           (codex-ide-status-mode--plain-text last-response)))
         (codex-ide-status-mode--apply-expanded-content-face start (point))))
     t)))

(defun codex-ide-status-mode--insert-thread-section (thread directory layout)
  "Insert a child section for THREAD in DIRECTORY using LAYOUT."
  (let* ((session (codex-ide-status-mode--thread-session thread directory))
         (status (if session
                     (codex-ide-session-status session)
                   "stored"))
         (label (codex-ide-renderer-status-label status))
         (thread-id (alist-get 'id thread))
         (raw-preview (or (alist-get 'name thread)
                          (alist-get 'preview thread)
                          "Untitled"))
         (first-prompt (when session
                         (codex-ide-status-mode--first-submitted-prompt-text session)))
         (prompt-count (when session
                         (codex-ide-status-mode--submitted-prompt-count session)))
         (last-prompt (when session
                        (codex-ide-status-mode--last-submitted-prompt-text session)))
         (buffer-name (when session
                        (buffer-name (codex-ide-session-buffer session))))
         (last-response (when session
                          (codex-ide-status-mode--buffer-transcript-slice session)))
         (updated-text (or (codex-ide-human-time-ago (alist-get 'updatedAt thread)) ""))
         (status-width (plist-get layout :status-width))
         (updated-width (plist-get layout :updated-width))
         (preview (codex-ide-status-mode--preview-line
                   (or first-prompt raw-preview)))
         (title (concat
                 (codex-ide-status-mode--format-heading-status
                  (codex-ide-status-mode--pad-heading-part label status-width)
                  status)
                 "  "
                 (codex-ide-status-mode--format-heading-updated
                  (codex-ide-status-mode--pad-heading-part updated-text updated-width))
                 "  "
                 (codex-ide-status-mode--format-heading-preview preview))))
    (codex-ide-section-insert
     'thread thread title
     (lambda (_section)
       (let ((start (point)))
         (codex-ide-status-mode--insert-thread-metadata-line "Thread ID" thread-id)
         (codex-ide-status-mode--insert-thread-metadata-line
          "Created"
          (codex-ide--format-thread-updated-at (alist-get 'createdAt thread)))
         (codex-ide-status-mode--insert-thread-metadata-line
          "Updated"
          (codex-ide--format-thread-updated-at (alist-get 'updatedAt thread)))
         (when last-prompt
           (codex-ide-status-mode--insert-buffer-metadata-line session)
           (codex-ide-status-mode--insert-thread-metadata-line
            "Number of Prompts"
            (number-to-string prompt-count))
           (codex-ide-status-mode--insert-thread-metadata-line
            "Last Prompt"
            (codex-ide-status-mode--preview-line last-prompt))
           (codex-ide-status-mode--insert-thread-metadata-line
            "Last Response"
            (codex-ide-status-mode--preview-line
             (codex-ide-status-mode--plain-text (or last-response "")))))
         (codex-ide-status-mode--apply-expanded-content-face start (point))))
     t)))

(defun codex-ide-status-mode--live-query-session ()
  "Return a live query session for the current status buffer."
  (if (and codex-ide-status-mode--query-session
           (process-live-p
            (codex-ide-session-process codex-ide-status-mode--query-session)))
      codex-ide-status-mode--query-session
    (setq codex-ide-status-mode--query-session
          (codex-ide--ensure-query-session-for-thread-selection
           codex-ide-status-mode--directory))))

(defun codex-ide-status-mode--request-page (&optional cursor)
  "Request a thread page for the current status scope after CURSOR."
  (codex-ide--list-threads-page
   (codex-ide-status-mode--live-query-session)
   :all-directories (codex-ide-status-mode--all-sessions-p)
   :cursor cursor
   :limit codex-ide-thread-list-default-limit
   :sort-key "updated_at"))

(defun codex-ide-status-mode--reload-threads ()
  "Reload the first page of threads for the current status scope."
  (let ((page (codex-ide-status-mode--request-page)))
    (setq codex-ide-status-mode--threads
          (append (alist-get 'data page) nil)
          codex-ide-status-mode--next-cursor
          (alist-get 'nextCursor page))))

(defun codex-ide-status-mode--append-threads (threads)
  "Append previously unseen THREADS to the current status cache."
  (let ((known (make-hash-table :test #'equal))
        (new nil))
    (dolist (thread codex-ide-status-mode--threads)
      (puthash (alist-get 'id thread) t known))
    (dolist (thread threads)
      (unless (gethash (alist-get 'id thread) known)
        (puthash (alist-get 'id thread) t known)
        (push thread new)))
    (setq codex-ide-status-mode--threads
          (append codex-ide-status-mode--threads (nreverse new)))))

(defun codex-ide-status-mode--insert-show-more-button ()
  "Insert an action for loading the next page when one is available."
  (when codex-ide-status-mode--next-cursor
    (unless (bolp)
      (insert "\n"))
    (insert "\n")
    (insert-text-button
     "Show more sessions"
     'follow-link t
     'help-echo "Load the next page of Codex sessions"
     'action (lambda (_button)
               (codex-ide-status-mode-show-more)))
    (insert "\n")))

(cl-defun codex-ide-status-mode--render-sections
    (directory &key (is-refresh nil) (reload t))
  "Render status sections for DIRECTORY and return the session count.

When IS-REFRESH is non-nil, existing buffer content will be erased/reset.
When RELOAD is non-nil, reload thread metadata before rendering."
  (codex-ide--prepare-session-operations)
  (when reload
    (codex-ide-status-mode--reload-threads))
  (when is-refresh
    (erase-buffer)
    (codex-ide-section-reset))
  (codex-ide-status-mode--refresh-striped-heading-face)
  (let* ((threads
          (codex-ide-status-mode--sort-threads-by-updated
           (codex-ide-status-mode--visible-threads)))
         (layout (codex-ide-status-mode--heading-layout threads directory))
         (index 0))
    (dolist (thread threads)
      (setq index (1+ index))
      (let* ((thread-directory
              (if (codex-ide-status-mode--all-sessions-p)
                  (codex-ide-status-mode--thread-directory thread)
                directory))
             (section
              (codex-ide-status-mode--insert-thread-section
               thread thread-directory layout)))
        (when (zerop (% index 2))
          (codex-ide-status-mode--apply-heading-stripe section))))
    (codex-ide-status-mode--insert-show-more-button)
    (length threads)))

(cl-defun codex-ide-status-mode--render-buffer
    (directory &key (is-refresh nil) (reload t))
  "Render the status buffer for DIRECTORY.
When RELOAD is non-nil, reload thread metadata before rendering."
  (let ((inhibit-read-only t))
    (setq-local header-line-format
                (codex-ide-status-mode--header-line
                 directory
                 (codex-ide-status-mode--render-sections directory
                                                         :is-refresh is-refresh
                                                         :reload reload)))
    (goto-char (point-min))))

;;;###autoload
(defun codex-ide-status-mode-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the current Codex status buffer."
  (interactive)
  (unless codex-ide-status-mode--directory
    (user-error "No Codex project is associated with this buffer"))
  (codex-ide-section-preserve-view-state
   #'codex-ide-status-mode--section-identity
   (lambda ()
     (codex-ide-status-mode--render-buffer codex-ide-status-mode--directory
                                           :is-refresh t))))

(defun codex-ide-status-mode-filter (filter)
  "Show only loaded sessions matching FILTER.
An empty FILTER clears the current filter."
  (interactive
   (list (read-string "Filter Codex sessions (empty clears): "
                      codex-ide-status-mode--filter)))
  (setq codex-ide-status-mode--filter (string-trim filter))
  (codex-ide-section-preserve-view-state
   #'codex-ide-status-mode--section-identity
   (lambda ()
     (codex-ide-status-mode--render-buffer
      codex-ide-status-mode--directory :is-refresh t :reload nil))))

(defun codex-ide-status-mode-quit ()
  "Clear the current filter, or quit the status window when unfiltered."
  (interactive)
  (if (string-empty-p codex-ide-status-mode--filter)
      (quit-window)
    (setq codex-ide-status-mode--filter "")
    (codex-ide-section-preserve-view-state
     #'codex-ide-status-mode--section-identity
     (lambda ()
       (codex-ide-status-mode--render-buffer
        codex-ide-status-mode--directory :is-refresh t :reload nil)))
    (message "Codex session filter cleared")))

(defun codex-ide-status-mode-show-more ()
  "Load and display the next page in the current status buffer."
  (interactive)
  (unless codex-ide-status-mode--next-cursor
    (user-error "No more Codex sessions are available"))
  (let ((page
         (codex-ide-status-mode--request-page
          codex-ide-status-mode--next-cursor)))
    (codex-ide-status-mode--append-threads
     (append (alist-get 'data page) nil))
    (setq codex-ide-status-mode--next-cursor
          (alist-get 'nextCursor page))
    (codex-ide-section-preserve-view-state
     #'codex-ide-status-mode--section-identity
     (lambda ()
       (codex-ide-status-mode--render-buffer
        codex-ide-status-mode--directory :is-refresh t :reload nil)))))

;;;###autoload
(defun codex-ide-status ()
  "Show the Codex status buffer for the current project."
  (interactive)
  (let* ((directory (codex-ide--normalize-directory
                     (codex-ide--get-working-directory)))
         (buffer-name (format "codex-ide: %s"
                              (codex-ide--project-name directory)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (codex-ide-status-mode)
      (setq-local default-directory directory)
      (setq-local codex-ide-status-mode--directory directory
                  codex-ide-status-mode--scope 'project
                  codex-ide-status-mode--filter ""
                  codex-ide-status-mode--query-session nil)
      (codex-ide-status-mode--render-buffer directory :is-refresh t))
    (pop-to-buffer buffer)))

;;;###autoload
(defun codex-ide-status-all ()
  "Show the Codex status buffer for sessions from all recorded directories."
  (interactive)
  (let* ((directory
          (codex-ide--normalize-directory
           (codex-ide--get-working-directory)))
         (buffer (get-buffer-create "*Codex Sessions*")))
    (with-current-buffer buffer
      (codex-ide-status-mode)
      (setq-local default-directory directory
                  codex-ide-status-mode--directory directory
                  codex-ide-status-mode--scope 'all
                  codex-ide-status-mode--filter ""
                  codex-ide-status-mode--query-session nil)
      (codex-ide-status-mode--render-buffer directory :is-refresh t))
    (pop-to-buffer buffer)))

(provide 'codex-ide-status-mode)

;;; codex-ide-status-mode.el ends here
