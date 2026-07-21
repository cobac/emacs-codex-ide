;;; codex-ide-resume-list.el --- List resumable Codex sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Tabulated listing of persisted Codex threads across all working directories.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'codex-ide)
(require 'codex-ide-session-list)

(declare-function evil-define-key* "evil-core"
                  (state keymap key def &rest bindings))

(defvar codex-ide-resume-list-mode-map
  (make-sparse-keymap)
  "Keymap for `codex-ide-resume-list-mode'.")

(set-keymap-parent codex-ide-resume-list-mode-map
                   codex-ide-session-list-mode-map)
(define-key codex-ide-resume-list-mode-map
            (kbd "/")
            #'codex-ide-resume-list-filter)
(define-key codex-ide-resume-list-mode-map
            (kbd "s")
            #'codex-ide-resume-list-filter)
(define-key codex-ide-resume-list-mode-map
            (kbd "+")
            #'codex-ide-resume-list-show-more)
(define-key codex-ide-resume-list-mode-map
            (kbd "TAB")
            #'codex-ide-resume-list-toggle-preview)
(define-key codex-ide-resume-list-mode-map
            (kbd "<tab>")
            #'codex-ide-resume-list-toggle-preview)
(define-key codex-ide-resume-list-mode-map
            (kbd "C-g")
            #'codex-ide-resume-list-clear-filter)
(define-key codex-ide-resume-list-mode-map
            (kbd "q")
            #'codex-ide-resume-list-quit)
(define-key codex-ide-resume-list-mode-map (kbd "g") nil)
(define-key codex-ide-resume-list-mode-map
            (kbd "g r")
            #'codex-ide-resume-list-refresh)

(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) codex-ide-resume-list-mode-map
    (kbd "/") #'codex-ide-resume-list-filter
    (kbd "s") #'codex-ide-resume-list-filter
    (kbd "TAB") #'codex-ide-resume-list-toggle-preview
    (kbd "C-g") #'codex-ide-resume-list-clear-filter
    (kbd "q") #'codex-ide-resume-list-quit))

(defvar-local codex-ide-resume-list--threads nil
  "Persisted thread metadata loaded into the current list buffer.")

(defvar-local codex-ide-resume-list--next-cursor nil
  "Cursor for the next page of persisted threads, or nil at the end.")

(defvar-local codex-ide-resume-list--filter ""
  "Case-insensitive filter applied to loaded persisted threads.")

(defvar-local codex-ide-resume-list--expanded-thread-ids nil
  "Hash table of thread ids whose full previews are expanded.")

(defvar-local codex-ide-resume-list--query-session nil
  "App-server session used to query persisted thread metadata.")

(defvar-local codex-ide-resume-list--query-directory nil
  "Directory used to create a replacement query session when needed.")

(defconst codex-ide-resume-list--table-fixed-width 70
  "Combined width of list padding and non-preview columns.")

(defconst codex-ide-resume-list--preview-min-width 20
  "Minimum width of the compact preview column.")

(define-derived-mode codex-ide-resume-list-mode codex-ide-session-list-mode
  "Codex-Resume"
  "Mode for listing persisted Codex sessions from all directories."
  (setq-local tabulated-list-use-header-line nil)
  (setq-local tabulated-list-printer
              #'codex-ide-resume-list--print-entry)
  (setq-local codex-ide-resume-list--expanded-thread-ids
              (make-hash-table :test #'equal))
  (add-hook 'window-size-change-functions
            #'codex-ide-resume-list--resize-columns nil t))

(defun codex-ide-resume-list--full-preview (thread)
  "Return THREAD's complete human-authored preview."
  (let ((preview
         (codex-ide--thread-choice-preview
          (or (alist-get 'preview thread) ""))))
    (if (string-empty-p preview) "Untitled" preview)))

(defun codex-ide-resume-list--preview (thread)
  "Return THREAD's human-authored preview for display and filtering."
  (let ((preview (codex-ide-resume-list--full-preview thread)))
    (setq preview (replace-regexp-in-string "[\n\r]+" "↵" preview))
    preview))

(defun codex-ide-resume-list--format-created-at (created-at)
  "Format CREATED-AT as a local date and time."
  (condition-case nil
      (format-time-string
       "%Y-%m-%d %H:%M"
       (if (numberp created-at)
           (seconds-to-time created-at)
         (date-to-time created-at)))
    (error (if created-at (format "%s" created-at) ""))))

(defun codex-ide-resume-list--filter-match-p (thread)
  "Return non-nil when THREAD matches the active list filter."
  (or (string-empty-p codex-ide-resume-list--filter)
      (let ((search-text
             (downcase
              (string-join
               (list (codex-ide-resume-list--preview thread)
                     (or (alist-get 'cwd thread) "")
                     (or (alist-get 'id thread) ""))
               "\n"))))
        (string-match-p
         (regexp-quote (downcase codex-ide-resume-list--filter))
         search-text))))

(defun codex-ide-resume-list--entries ()
  "Return tabulated entries for loaded persisted Codex sessions."
  (mapcar
   (lambda (thread)
     (let ((thread-id (or (alist-get 'id thread) ""))
           (directory (or (alist-get 'cwd thread) "")))
       (list (list thread-id directory)
             (vector
              (codex-ide-session-list-cell
               (codex-ide--format-thread-updated-at
                (alist-get 'updatedAt thread))
               'codex-ide-session-list-time-face)
              (codex-ide-session-list-cell
               (codex-ide-resume-list--preview thread)
               'default)
              (codex-ide-session-list-cell
               (if (string-empty-p directory)
                   "Unknown"
                 (abbreviate-file-name directory))
               'codex-ide-session-list-secondary-face)
              (codex-ide-session-list-cell
               (codex-ide-resume-list--format-created-at
                (alist-get 'createdAt thread))
               'codex-ide-session-list-time-face)))))
   (seq-filter #'codex-ide-resume-list--filter-match-p
               codex-ide-resume-list--threads)))

(defun codex-ide-resume-list--table-format
    (window-width &optional minimum-preview-width)
  "Return a table format sized for WINDOW-WIDTH columns.
Keep the preview at least MINIMUM-PREVIEW-WIDTH columns wide when provided."
  (let ((preview-width
         (max codex-ide-resume-list--preview-min-width
              (or minimum-preview-width 0)
              (- window-width codex-ide-resume-list--table-fixed-width))))
    (vector
     (list "Updated" 16 #'codex-ide-resume-list--updated-less-p)
     (list "Preview" preview-width t)
     (list "Directory" 36 t)
     (list "Created" 16 t))))

(defun codex-ide-resume-list--display-width ()
  "Return the narrowest visible window width for the current buffer."
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (if windows
        (apply #'min (mapcar #'window-body-width windows))
      (window-body-width (selected-window)))))

(defun codex-ide-resume-list--resize-columns (&optional _window)
  "Grow the preview column to fill the visible list width."
  (when (and tabulated-list-format
             (get-buffer-window (current-buffer) t))
    (let* ((preview-width (nth 1 (aref tabulated-list-format 1)))
           (format
            (codex-ide-resume-list--table-format
             (codex-ide-resume-list--display-width)
             preview-width)))
      (unless (equal format tabulated-list-format)
        (setq tabulated-list-format format)
        (tabulated-list-init-header)
        (tabulated-list-print t)))))

(defun codex-ide-resume-list--thread (thread-id)
  "Return the loaded thread identified by THREAD-ID."
  (seq-find (lambda (thread)
              (equal (alist-get 'id thread) thread-id))
            codex-ide-resume-list--threads))

(defun codex-ide-resume-list--timestamp-value (value)
  "Return VALUE as a numeric timestamp, or zero when it is invalid."
  (cond
   ((numberp value) value)
   ((stringp value)
    (condition-case nil
        (float-time (date-to-time value))
      (error 0)))
   (t 0)))

(defun codex-ide-resume-list--updated-less-p (entry-a entry-b)
  "Return non-nil when ENTRY-A was updated before ENTRY-B."
  (let* ((thread-id-a (car-safe (car entry-a)))
         (thread-id-b (car-safe (car entry-b)))
         (updated-a
          (codex-ide-resume-list--timestamp-value
           (alist-get 'updatedAt
                      (codex-ide-resume-list--thread thread-id-a))))
         (updated-b
          (codex-ide-resume-list--timestamp-value
           (alist-get 'updatedAt
                      (codex-ide-resume-list--thread thread-id-b)))))
    (if (= updated-a updated-b)
        (string-lessp (or thread-id-a "") (or thread-id-b ""))
      (< updated-a updated-b))))

(defun codex-ide-resume-list--expanded-preview-width ()
  "Return the text width available for an expanded preview."
  (let ((window (or (get-buffer-window (current-buffer) t)
                    (selected-window))))
    (max 20 (- (window-body-width window) 6))))

(defun codex-ide-resume-list--wrap-preview-line (line width)
  "Wrap LINE to WIDTH columns, breaking long words when necessary."
  (let ((remaining line)
        lines)
    (while (> (string-width remaining) width)
      (let* ((prefix (truncate-string-to-width remaining width nil nil ""))
             (break
              (and (string-match "[ \t]+[^ \t]*\\'" prefix)
                   (> (match-beginning 0) 0)
                   (match-beginning 0))))
        (if break
            (progn
              (push (string-trim-right (substring remaining 0 break)) lines)
              (setq remaining
                    (string-trim-left (substring remaining break))))
          (push prefix lines)
          (setq remaining (substring remaining (length prefix))))))
    (push remaining lines)
    (string-join (nreverse lines) "\n")))

(defun codex-ide-resume-list--wrap-preview (preview)
  "Wrap PREVIEW to the current list window without joining source lines."
  (let ((width (codex-ide-resume-list--expanded-preview-width)))
    (mapconcat
     (lambda (line)
       (codex-ide-resume-list--wrap-preview-line line width))
     (split-string preview "\n" nil)
     "\n")))

(defun codex-ide-resume-list--add-expanded-preview (thread)
  "Add THREAD's expanded preview after the row at point."
  (let ((overlay (make-overlay (line-beginning-position)
                               (line-end-position))))
    (overlay-put overlay 'codex-ide-resume-list-preview t)
    (overlay-put overlay 'evaporate t)
    (overlay-put
     overlay
     'after-string
     (concat
      "\n"
      (propertize
       (concat
        (replace-regexp-in-string
         "^" "    "
         (codex-ide-resume-list--wrap-preview
          (codex-ide-resume-list--full-preview thread)))
        "\n")
       'face 'default)
      (propertize
       (replace-regexp-in-string
        "^" "    "
        (codex-ide-resume-list--wrap-preview
         (format "Thread ID: %s" (or (alist-get 'id thread) "Unknown"))))
       'face 'codex-ide-session-list-id-face)))))

(defun codex-ide-resume-list--print-entry (id columns)
  "Print a resumable-session row for ID and COLUMNS."
  (let* ((thread-id (car-safe id))
         (expanded (and thread-id
                        (gethash thread-id
                                 codex-ide-resume-list--expanded-thread-ids)))
         (display-columns (if expanded (copy-sequence columns) columns))
         (row-start (point)))
    (when expanded
      (aset display-columns 1
            (codex-ide-session-list-cell "" 'default)))
    (tabulated-list-print-entry id display-columns)
    (when-let* ((expanded)
                (thread (codex-ide-resume-list--thread thread-id)))
      (save-excursion
        (goto-char row-start)
        (codex-ide-resume-list--add-expanded-preview thread)))))

(defun codex-ide-resume-list--apply-expanded-previews ()
  "Show full previews beneath expanded rows in the current list."
  (remove-overlays (point-min) (point-max)
                   'codex-ide-resume-list-preview t)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (when-let* ((row-id (tabulated-list-get-id))
                  (thread-id (car row-id))
                  ((gethash thread-id
                            codex-ide-resume-list--expanded-thread-ids))
                  (thread (codex-ide-resume-list--thread thread-id)))
        (codex-ide-resume-list--add-expanded-preview thread))
      (forward-line 1))))

(defun codex-ide-resume-list-toggle-preview ()
  "Toggle the complete `thread/list' preview beneath the row at point."
  (interactive)
  (unless codex-ide-resume-list--expanded-thread-ids
    (setq codex-ide-resume-list--expanded-thread-ids
          (make-hash-table :test #'equal)))
  (when-let* ((row-id (tabulated-list-get-id))
              (thread-id (car row-id)))
    (if (gethash thread-id codex-ide-resume-list--expanded-thread-ids)
        (remhash thread-id codex-ide-resume-list--expanded-thread-ids)
      (puthash thread-id t codex-ide-resume-list--expanded-thread-ids))
    (tabulated-list-print t)))

(defun codex-ide-resume-list--live-query-session ()
  "Return a live query session for the current persisted-session list."
  (if (and codex-ide-resume-list--query-session
           (process-live-p
            (codex-ide-session-process codex-ide-resume-list--query-session)))
      codex-ide-resume-list--query-session
    (let ((default-directory
           (file-name-as-directory codex-ide-resume-list--query-directory)))
      (setq codex-ide-resume-list--query-session
            (codex-ide--ensure-query-session-for-thread-selection
             codex-ide-resume-list--query-directory)))))

(defun codex-ide-resume-list--request-page (&optional cursor)
  "Request a persisted-thread page after CURSOR for the current list."
  (codex-ide--list-threads-page
   (codex-ide-resume-list--live-query-session)
   :all-directories t
   :cursor cursor
   :limit codex-ide-thread-list-default-limit
   :sort-key "updated_at"))

(defun codex-ide-resume-list--append-threads (threads)
  "Append previously unseen THREADS to the current list cache."
  (let ((known (make-hash-table :test #'equal))
        (new nil))
    (dolist (thread codex-ide-resume-list--threads)
      (puthash (alist-get 'id thread) t known))
    (dolist (thread threads)
      (unless (gethash (alist-get 'id thread) known)
        (puthash (alist-get 'id thread) t known)
        (push thread new)))
    (setq codex-ide-resume-list--threads
          (append codex-ide-resume-list--threads (nreverse new)))))

(defun codex-ide-resume-list--render ()
  "Render the current persisted-session list state."
  (tabulated-list-print t)
  (codex-ide-resume-list--apply-expanded-previews)
  (message "Showing %d of %d loaded Codex session%s%s"
           (length (codex-ide-resume-list--entries))
           (length codex-ide-resume-list--threads)
           (if (= (length codex-ide-resume-list--threads) 1) "" "s")
           (if codex-ide-resume-list--next-cursor
               "; press + to show more"
             "")))

(defun codex-ide-resume-list-filter (filter)
  "Show only loaded sessions matching FILTER.
An empty FILTER clears the current filter."
  (interactive
   (list (read-string "Filter Codex sessions (empty clears): "
                      codex-ide-resume-list--filter)))
  (setq codex-ide-resume-list--filter (string-trim filter))
  (codex-ide-resume-list--render))

(defun codex-ide-resume-list-clear-filter ()
  "Clear the current session filter, or quit when none is active."
  (interactive)
  (if (string-empty-p codex-ide-resume-list--filter)
      (keyboard-quit)
    (setq codex-ide-resume-list--filter "")
    (codex-ide-resume-list--render)
    (message "Codex session filter cleared")))

(defun codex-ide-resume-list-quit ()
  "Clear the current filter, or quit the list window when unfiltered."
  (interactive)
  (if (string-empty-p codex-ide-resume-list--filter)
      (quit-window)
    (codex-ide-resume-list-clear-filter)))

(defun codex-ide-resume-list-show-more ()
  "Load and display the next page of persisted Codex sessions."
  (interactive)
  (unless codex-ide-resume-list--next-cursor
    (user-error "No more Codex sessions are available"))
  (let ((page (codex-ide-resume-list--request-page
               codex-ide-resume-list--next-cursor)))
    (codex-ide-resume-list--append-threads
     (append (alist-get 'data page) nil))
    (setq codex-ide-resume-list--next-cursor
          (alist-get 'nextCursor page))
    (codex-ide-resume-list--render)))

(defun codex-ide-resume-list-refresh ()
  "Reload the first page of persisted Codex sessions."
  (interactive)
  (let ((page (codex-ide-resume-list--request-page)))
    (setq codex-ide-resume-list--threads
          (append (alist-get 'data page) nil)
          codex-ide-resume-list--next-cursor
          (alist-get 'nextCursor page))
    (codex-ide-resume-list--render)))

(defun codex-ide-resume-list--visit (row-id)
  "Resume the persisted thread represented by ROW-ID."
  (let ((thread-id (car row-id))
        (directory (cadr row-id)))
    (unless (and (stringp directory) (not (string-empty-p directory)))
      (user-error "Stored Codex session has no working directory"))
    (codex-ide--show-or-resume-thread thread-id directory)))

;;;###autoload
(defun codex-ide-resume-list ()
  "Show resumable Codex sessions from all recorded directories."
  (interactive)
  (codex-ide--prepare-session-operations)
  (let* ((query-directory (codex-ide--get-working-directory))
         (query-session
          (codex-ide--ensure-query-session-for-thread-selection query-directory))
         (page (codex-ide--list-threads-page
                query-session
                :all-directories t
                :limit codex-ide-thread-list-default-limit
                :sort-key "updated_at"))
         (threads (append (alist-get 'data page) nil))
         (next-cursor (alist-get 'nextCursor page))
         (buffer
          (codex-ide-session-list--setup
           "*Codex Past Sessions*"
           #'codex-ide-resume-list-mode
           (codex-ide-resume-list--table-format
            (window-body-width (selected-window)))
           #'codex-ide-resume-list--entries
           #'codex-ide-resume-list--visit
           nil
           (lambda ()
             (setq codex-ide-resume-list--threads threads
                   codex-ide-resume-list--next-cursor next-cursor
                   codex-ide-resume-list--filter ""
                   codex-ide-resume-list--expanded-thread-ids
                   (make-hash-table :test #'equal)
                   codex-ide-resume-list--query-session query-session
                   codex-ide-resume-list--query-directory query-directory)))))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (codex-ide-resume-list--resize-columns))
    (message "Loaded %d Codex session%s%s"
             (length threads)
             (if (= (length threads) 1) "" "s")
             (if next-cursor "; press + to show more" ""))))

(provide 'codex-ide-resume-list)

;;; codex-ide-resume-list.el ends here
