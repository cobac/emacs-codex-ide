;;; codex-ide-resume-list-tests.el --- Tests for persisted session list -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-resume-list'.

;;; Code:

(require 'ert)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-resume-list)

(ert-deftest codex-ide-list-threads-page-can-list-all-directories ()
  (let ((captured-params nil)
        (session (make-codex-ide-session :directory "/tmp/project")))
    (cl-letf (((symbol-function 'codex-ide--request-sync)
               (lambda (_session method params)
                 (should (equal method "thread/list"))
                 (setq captured-params params)
                 '((data . []) (nextCursor . "next-page")))))
      (should
       (equal
        (codex-ide--list-threads-page
         session
         :all-directories t
         :cursor "current-page"
         :limit 100)
        '((data . []) (nextCursor . "next-page"))))
      (should
       (equal captured-params
              '((cursor . "current-page")
                (limit . 100)
                (sortKey . "updated_at")))))))

(ert-deftest codex-ide-resume-list-binds-filter-and-pagination-commands ()
  (should (eq (lookup-key codex-ide-resume-list-mode-map (kbd "/"))
              #'codex-ide-resume-list-filter))
  (should (eq (lookup-key codex-ide-resume-list-mode-map (kbd "s"))
              #'codex-ide-resume-list-filter))
  (should (eq (lookup-key codex-ide-resume-list-mode-map (kbd "+"))
              #'codex-ide-resume-list-show-more))
  (should (eq (lookup-key codex-ide-resume-list-mode-map (kbd "TAB"))
              #'codex-ide-resume-list-toggle-preview))
  (should (eq (lookup-key codex-ide-resume-list-mode-map (kbd "C-g"))
              #'codex-ide-resume-list-clear-filter))
  (should (eq (lookup-key codex-ide-resume-list-mode-map (kbd "q"))
              #'codex-ide-resume-list-quit))
  (should-not (eq (lookup-key codex-ide-resume-list-mode-map (kbd "g"))
                  #'codex-ide-resume-list-refresh))
  (should (eq (lookup-key codex-ide-resume-list-mode-map (kbd "g r"))
              #'codex-ide-resume-list-refresh)))

(ert-deftest codex-ide-resume-list-renders-headings-in-buffer ()
  (with-temp-buffer
    (codex-ide-resume-list-mode)
    (should-not tabulated-list-use-header-line)
    (setq tabulated-list-format
          (codex-ide-resume-list--table-format 118)
          tabulated-list-entries nil)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (should-not header-line-format)
    (goto-char (point-min))
    (search-forward "Updated")
    (should (equal (get-text-property (- (point) (length "Updated"))
                                      'tabulated-list-column-name)
                   "Updated"))))

(ert-deftest codex-ide-resume-list-preview-column-fills-window ()
  (let* ((window-width 180)
         (format (codex-ide-resume-list--table-format window-width))
         (preview (aref format 1)))
    (should (= (length format) 4))
    (should (equal (mapcar #'car (append format nil))
                   '("Updated" "Preview" "Directory" "Created")))
    (should (= (nth 1 preview)
               (- window-width codex-ide-resume-list--table-fixed-width)))
    (should (> (nth 1 preview) 48))))

(ert-deftest codex-ide-resume-list-preview-column-grows-but-does-not-shrink ()
  (let* ((original-width 110)
         (narrow-format
          (codex-ide-resume-list--table-format 90 original-width))
         (wide-format
          (codex-ide-resume-list--table-format 220 original-width)))
    (should (= (nth 1 (aref narrow-format 1)) original-width))
    (should (= (nth 1 (aref wide-format 1))
               (- 220 codex-ide-resume-list--table-fixed-width)))))

(ert-deftest codex-ide-resume-list-preview-and-filter-hide-emacs-context ()
  (let ((thread
         `((id . "thread-human")
           (cwd . "/tmp/example")
           (createdAt . 1744030000)
           (updatedAt . 1744038896)
           (preview
            . ,(concat
                "[Emacs session context]\n"
                "Secret system-only phrase.\n"
                "[/Emacs session context]\n\n"
                "[Emacs prompt context]\n"
                "Buffer: example.el\n"
                "[/Emacs prompt context]\n\n"
                "Investigate the failing test")))))
    (with-temp-buffer
      (codex-ide-resume-list-mode)
      (setq codex-ide-resume-list--threads (list thread))
      (should (equal (codex-ide-resume-list--preview thread)
                     "Investigate the failing test"))
      (should (equal (codex-ide-resume-list--full-preview thread)
                     "Investigate the failing test"))
      (let ((columns (cadr (car (codex-ide-resume-list--entries)))))
        (should (= (length columns) 4))
        (should (eq (get-text-property 0 'face (aref columns 1))
                    'default))
        (should (stringp (aref columns 3)))
        (should-not (seq-find (lambda (column)
                               (string-match-p "thread-human" column))
                             columns)))
      (setq codex-ide-resume-list--filter "failing")
      (should (= (length (codex-ide-resume-list--entries)) 1))
      (setq codex-ide-resume-list--filter "system-only")
      (should-not (codex-ide-resume-list--entries)))))

(ert-deftest codex-ide-resume-list-expanded-preview-preserves-lines ()
  (let ((thread '((id . "thread-1")
                  (cwd . "/tmp/example")
                  (preview . "First line\nSecond line"))))
    (with-temp-buffer
      (codex-ide-resume-list-mode)
      (setq codex-ide-resume-list--threads (list thread)
            tabulated-list-format
            (codex-ide-resume-list--table-format 118)
            tabulated-list-entries #'codex-ide-resume-list--entries)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (forward-line 1)
      (should (string-match-p "First line↵Second line" (buffer-string)))
      (codex-ide-resume-list-toggle-preview)
      (should (gethash "thread-1"
                       codex-ide-resume-list--expanded-thread-ids))
      (should-not (string-match-p "First line↵Second line" (buffer-string)))
      (let ((detail
             (overlay-get
              (car (overlays-in (line-beginning-position)
                                (1+ (line-end-position))))
              'after-string)))
        (should (string-match-p "First line\n    Second line" detail))
        (should (string-match-p "Thread ID: thread-1" detail))
        (should (eq (get-text-property
                     (string-match "First line" detail) 'face detail)
                    'default))
        (should (< (string-match "First line" detail)
                   (string-match "Thread ID:" detail))))
      (codex-ide-resume-list-toggle-preview)
      (should-not (gethash "thread-1"
                           codex-ide-resume-list--expanded-thread-ids))
      (should (string-match-p "First line↵Second line" (buffer-string))))))

(ert-deftest codex-ide-resume-list-expanded-preview-wraps-to-window ()
  (should
   (equal
    (codex-ide-resume-list--wrap-preview-line
     "alpha beta gamma delta" 12)
    "alpha beta\ngamma delta"))
  (let ((wrapped
         (codex-ide-resume-list--wrap-preview-line
          "abcdefghijklmnopqrst" 7)))
    (dolist (line (split-string wrapped "\n"))
      (should (<= (string-width line) 7)))))

(ert-deftest codex-ide-resume-list-reprints-expanded-preview-after-sort ()
  (let ((first '((id . "thread-1")
                 (cwd . "/tmp/one")
                 (updatedAt . 100)
                 (preview . "Zulu preview")))
        (second '((id . "thread-2")
                  (cwd . "/tmp/two")
                  (updatedAt . 200)
                  (preview . "Alpha preview"))))
    (with-temp-buffer
      (codex-ide-resume-list-mode)
      (setq codex-ide-resume-list--threads (list first second)
            tabulated-list-format
            (codex-ide-resume-list--table-format 118)
            tabulated-list-entries #'codex-ide-resume-list--entries)
      (puthash "thread-1" t codex-ide-resume-list--expanded-thread-ids)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (should (seq-some
               (lambda (overlay)
                 (overlay-get overlay 'codex-ide-resume-list-preview))
               (overlays-in (point-min) (point-max))))
      (setq tabulated-list-sort-key '("Preview" . nil))
      (tabulated-list-print t)
      (should
       (= 1
          (seq-count
           (lambda (overlay)
             (overlay-get overlay 'codex-ide-resume-list-preview))
           (overlays-in (point-min) (point-max)))))
      (should-not
       (seq-some
        (lambda (overlay)
          (overlay-get overlay 'codex-ide-resume-list-preview))
        (overlays-at (point-min)))))))

(ert-deftest codex-ide-resume-list-updated-sort-uses-raw-timestamps ()
  (let ((older '((id . "older") (updatedAt . 100)))
        (newer '((id . "newer") (updatedAt . 200))))
    (with-temp-buffer
      (codex-ide-resume-list-mode)
      (setq codex-ide-resume-list--threads (list newer older))
      (should
       (codex-ide-resume-list--updated-less-p
        '(("older" "") ["2 months ago"])
        '(("newer" "") ["10 minutes ago"])))
      (should-not
       (codex-ide-resume-list--updated-less-p
        '(("newer" "") ["10 minutes ago"])
        '(("older" "") ["2 months ago"]))))))

(ert-deftest codex-ide-resume-list-clear-filter-restores-all-rows ()
  (let ((rendered nil))
    (with-temp-buffer
      (codex-ide-resume-list-mode)
      (setq codex-ide-resume-list--filter "needle")
      (cl-letf (((symbol-function 'codex-ide-resume-list--render)
                 (lambda () (setq rendered t))))
        (codex-ide-resume-list-clear-filter))
      (should (string-empty-p codex-ide-resume-list--filter))
      (should rendered))))

(ert-deftest codex-ide-resume-list-quit-clears-filter-before-quitting ()
  (let ((rendered nil)
        (quit nil))
    (with-temp-buffer
      (codex-ide-resume-list-mode)
      (setq codex-ide-resume-list--filter "needle")
      (cl-letf (((symbol-function 'codex-ide-resume-list--render)
                 (lambda () (setq rendered t)))
                ((symbol-function 'quit-window)
                 (lambda (&rest _args) (setq quit t))))
        (codex-ide-resume-list-quit))
      (should (string-empty-p codex-ide-resume-list--filter))
      (should rendered)
      (should-not quit))))

(ert-deftest codex-ide-resume-list-quit-closes-unfiltered-list ()
  (let ((quit nil))
    (with-temp-buffer
      (codex-ide-resume-list-mode)
      (cl-letf (((symbol-function 'quit-window)
                 (lambda (&rest _args) (setq quit t))))
        (codex-ide-resume-list-quit))
      (should quit))))

(ert-deftest codex-ide-resume-list-show-more-appends-next-page ()
  (let ((first '((id . "thread-1")
                 (cwd . "/tmp/one")
                 (preview . "First")))
        (second '((id . "thread-2")
                  (cwd . "/tmp/two")
                  (preview . "Second")))
        (requested-cursor nil)
        (rendered nil))
    (with-temp-buffer
      (codex-ide-resume-list-mode)
      (setq codex-ide-resume-list--threads (list first)
            codex-ide-resume-list--next-cursor "page-2")
      (cl-letf (((symbol-function 'codex-ide-resume-list--request-page)
                 (lambda (&optional cursor)
                   (setq requested-cursor cursor)
                   `((data . [,second]) (nextCursor . nil))))
                ((symbol-function 'codex-ide-resume-list--render)
                 (lambda () (setq rendered t))))
        (codex-ide-resume-list-show-more))
      (should (equal requested-cursor "page-2"))
      (should (equal (mapcar (lambda (thread) (alist-get 'id thread))
                             codex-ide-resume-list--threads)
                     '("thread-1" "thread-2")))
      (should-not codex-ide-resume-list--next-cursor)
      (should rendered))))

(ert-deftest codex-ide-resume-list-visit-forwards-thread-and-directory ()
  (let ((captured nil))
    (cl-letf (((symbol-function 'codex-ide--show-or-resume-thread)
               (lambda (thread-id directory)
                 (setq captured (list thread-id directory)))))
      (codex-ide-resume-list--visit '("thread-1" "/tmp/original")))
    (should (equal captured '("thread-1" "/tmp/original")))))

(ert-deftest codex-ide-create-process-session-for-directory-preserves-subdirectory ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "project" root-dir))
         (subdirectory (expand-file-name "packages/example" project-dir)))
    (make-directory (expand-file-name ".git" project-dir) t)
    (make-directory subdirectory t)
    (codex-ide-test-with-fixture project-dir
      (codex-ide-test-with-fake-processes
        (let ((fake-make-process (symbol-function 'make-process))
              (spawn-directory nil))
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq spawn-directory default-directory)
                       (apply fake-make-process args))))
            (let ((session
                   (codex-ide--create-process-session-for-directory subdirectory)))
              (should (equal (codex-ide-session-directory session)
                             (codex-ide--normalize-directory subdirectory)))
              (should (equal (directory-file-name spawn-directory)
                             (codex-ide--normalize-directory subdirectory)))
              (with-current-buffer (codex-ide-session-buffer session)
                (should (equal (directory-file-name default-directory)
                               (codex-ide--normalize-directory subdirectory)))))))))))

(ert-deftest codex-ide-thread-resume-params-use-explicit-session-directory ()
  (let* ((session-directory "/tmp/original/subdirectory")
         (session (make-codex-ide-session :directory session-directory)))
    (cl-letf (((symbol-function 'codex-ide--get-working-directory)
               (lambda () "/tmp/inferred-project-root"))
              ((symbol-function 'codex-ide-config-effective-value)
               (lambda (&rest _args) nil))
              ((symbol-function 'codex-ide-config-effective-reasoning-effort)
               (lambda (&optional _session) nil))
              ((symbol-function 'codex-ide--fast-service-tier)
               (lambda (&optional _session) nil)))
      (should
       (equal (alist-get 'cwd
                         (codex-ide--thread-resume-params "thread-1" session))
              session-directory)))))

(ert-deftest codex-ide-show-or-resume-thread-rejects-missing-directory ()
  (let ((created nil)
        (missing (expand-file-name "codex-ide-missing-directory" temporary-file-directory)))
    (cl-letf (((symbol-function 'codex-ide--create-process-session-for-directory)
               (lambda (_directory) (setq created t))))
      (should-error (codex-ide--show-or-resume-thread "thread-1" missing)
                    :type 'user-error))
    (should-not created)))

(provide 'codex-ide-resume-list-tests)

;;; codex-ide-resume-list-tests.el ends here
