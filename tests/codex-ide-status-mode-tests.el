;;; codex-ide-status-mode-tests.el --- Tests for codex-ide status mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-status-mode'.

;;; Code:

(require 'ert)
(require 'button)
(require 'codex-ide-test-fixtures)
(require 'codex-ide)
(require 'codex-ide-status-mode)

(defun codex-ide-status-mode-test--set-session-buffer-prompts (session prompts &optional active-prompt)
  "Populate SESSION buffer with PROMPTS and optional ACTIVE-PROMPT."
  (with-current-buffer (codex-ide-session-buffer session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (prompt prompts)
        (let ((start (point)))
          (insert "> " prompt)
          (codex-ide--style-user-prompt-region start (point))
          (insert "\n\nassistant\n\n"))))
    (codex-ide--insert-input-prompt session (or active-prompt ""))))

(defun codex-ide-status-mode-test--set-session-buffer-with-response
    (session prompts response-lines &optional active-prompt)
  "Populate SESSION with PROMPTS followed by RESPONSE-LINES and ACTIVE-PROMPT."
  (with-current-buffer (codex-ide-session-buffer session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (prompt prompts)
        (let ((start (point)))
          (insert "> " prompt)
          (codex-ide--style-user-prompt-region start (point))
          (insert "\n\n")
          (let ((codex-ide--current-agent-item-type "agentMessage"))
            (codex-ide--append-agent-text
             (current-buffer)
             (concat (mapconcat (lambda (line) (or line "")) response-lines "\n") "\n"))))))
    (codex-ide--insert-input-prompt session (or active-prompt ""))))

(defun codex-ide-status-mode-test--preview-text (text max-lines)
  "Return transcript preview text for TEXT using MAX-LINES."
  (with-temp-buffer
    (insert text)
    (let ((codex-ide-status-mode-transcript-preview-max-lines max-lines))
      (when-let* ((range (codex-ide-status-mode--transcript-preview-range
                          (point-min)
                          (point-max))))
        (buffer-substring-no-properties (car range) (cdr range))))))

(defun codex-ide-status-mode-test--transcript-text (&rest blocks)
  "Return transcript text containing BLOCKS separated like session output."
  (mapconcat
   (lambda (block-lines)
     (concat
      (codex-ide-renderer-output-separator-string)
      "\n"
      (mapconcat #'identity block-lines "\n")
      "\n"))
   blocks
   ""))

(defun codex-ide-status-mode-test--face-includes-p (value face)
  "Return non-nil when face VALUE includes FACE."
  (if (listp value)
      (memq face value)
    (eq value face)))

(defun codex-ide-status-mode-test--header-line-string ()
  "Return the current buffer header line as plain text."
  (if (stringp header-line-format)
      (substring-no-properties header-line-format)
    (substring-no-properties (format-mode-line header-line-format))))

(ert-deftest codex-ide-status-transcript-preview-expands-to-full-current-block ()
  (let* ((text (codex-ide-status-mode-test--transcript-text
                '("alpha 1" "alpha 2")
                '("beta 1" "beta 2" "beta 3")))
         (preview (codex-ide-status-mode-test--preview-text text 1)))
    (should (equal preview
                   (concat "beta 1\nbeta 2\nbeta 3")))))

(ert-deftest codex-ide-status-transcript-preview-starts-at-containing-block ()
  (let* ((text (codex-ide-status-mode-test--transcript-text
                '("alpha 1" "alpha 2")
                '("beta 1" "beta 2" "beta 3")
                '("gamma 1" "gamma 2")))
         (preview (codex-ide-status-mode-test--preview-text text 5)))
    (should (equal preview
                   (concat
                    "beta 1\nbeta 2\nbeta 3\n"
                    "------------------------------------------------------------------------\n\n"
                    "gamma 1\ngamma 2")))))

(ert-deftest codex-ide-status-mode-disables-line-wrapping-and-keeps-visual-line-motion ()
  (with-temp-buffer
    (visual-line-mode 1)
    (codex-ide-status-mode)
    (should truncate-lines)
    (should-not word-wrap)
    (should line-move-visual)
    (should-not visual-line-mode)))

(ert-deftest codex-ide-status-refresh-preserves-window-point-from-other-current-buffer ()
  (let ((status-buffer (get-buffer-create " *codex-ide-status-refresh-point*"))
        (other-buffer (get-buffer-create " *codex-ide-status-refresh-other*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer other-buffer)
          (let ((status-window (split-window-right)))
            (set-window-buffer status-window status-buffer)
            (with-current-buffer status-buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "alpha\nbeta\ngamma\n"))
              (codex-ide-status-mode)
              (setq-local codex-ide-status-mode--directory default-directory)
              (goto-char (point-min)))
            (set-window-point status-window (with-current-buffer status-buffer
                                              (save-excursion
						(goto-char (point-min))
						(forward-line 2)
						(point))))
            (cl-letf (((symbol-function 'codex-ide-status-mode--render-buffer)
                       (lambda (&rest _args)
                         (let ((inhibit-read-only t))
                           (goto-char (point-min))
                           (set-window-point status-window (point-min))))))
              (with-current-buffer status-buffer
                (codex-ide-status-mode-refresh)))
            (should (= (window-point status-window)
                       (with-current-buffer status-buffer
                         (save-excursion
                           (goto-char (point-min))
                           (forward-line 2)
                           (point)))))))
      (kill-buffer status-buffer)
      (kill-buffer other-buffer))))

(ert-deftest codex-ide-status-striped-heading-background-brightens-dark-themes ()
  (cl-letf (((symbol-function 'face-background)
             (lambda (&rest _args) "#101010"))
            ((symbol-function 'face-foreground)
             (lambda (&rest _args) "#f0f0f0")))
    (let ((codex-ide-status-mode-stripe-mix 0.12))
      (should (equal (codex-ide-status-mode--striped-heading-background)
                     "#1e1e1e")))))

(ert-deftest codex-ide-status-striped-heading-background-darkens-light-themes ()
  (cl-letf (((symbol-function 'face-background)
             (lambda (&rest _args) "#f0f0f0"))
            ((symbol-function 'face-foreground)
             (lambda (&rest _args) "#101010")))
    (let ((codex-ide-status-mode-stripe-mix 0.12))
      (should (equal (codex-ide-status-mode--striped-heading-background)
                     "#e0e0e0")))))

(ert-deftest codex-ide-status-striped-heading-background-respects-custom-mix ()
  (cl-letf (((symbol-function 'face-background)
             (lambda (&rest _args) "#101010"))
            ((symbol-function 'face-foreground)
             (lambda (&rest _args) "#f0f0f0")))
    (let ((subtle nil)
          (visible nil))
      (let ((codex-ide-status-mode-stripe-mix 0.0))
        (setq subtle (codex-ide-status-mode--striped-heading-background)))
      (let ((codex-ide-status-mode-stripe-mix 0.12))
        (setq visible (codex-ide-status-mode--striped-heading-background)))
      (should (stringp subtle))
      (should (stringp visible))
      (should-not (equal subtle visible)))))

(ert-deftest codex-ide-status-renders-project-sections-and-collapsed-entries ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (other-dir (expand-file-name "beta" root-dir))
         (threads `(((id . "thread-alpha")
                     (name . "Summarize\nsession")
                     (createdAt . 10)
                     (updatedAt . 20))
                    ((id . "thread-stored")
                     (preview . "Stored preview")
                     (createdAt . 30)
                     (updatedAt . 40)))))
    (make-directory project-dir t)
    (make-directory other-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil)
					(other-session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (let ((default-directory other-dir))
				      (setq other-session (codex-ide--create-process-session)))
				    (codex-ide-status-mode-test--set-session-buffer-with-response
				     session
				     '("Earlier prompt" "Explain\nfailure")
				     '("Assistant reply"))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "running"
					  (codex-ide-session-status other-session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(should (derived-mode-p 'codex-ide-status-mode))
					(should (equal (codex-ide-status-mode-test--header-line-string)
						       "Project: alpha | 2 sessions"))
					(goto-char (point-min))
					(should-not (looking-at-p "^$"))
					(should (or (looking-at-p "Stored  .*  Stored preview")
						    (looking-at-p "Running  .*  Earlier prompt")))
					(should (string-match-p "Running  .*  Earlier prompt" (buffer-string)))
					(should (string-match-p "Stored  .*  Stored preview" (buffer-string)))
					(should-not (string-match-p (regexp-quote (buffer-name (codex-ide-session-buffer other-session)))
								    (buffer-string)))
					(goto-char (point-min))
					(search-forward "Earlier prompt")
					(beginning-of-line)
					(forward-line 1)
					(should (invisible-p (point)))
					(goto-char (point-min))
					(search-forward "Stored preview")
					(beginning-of-line)
					(forward-line 1)
					(should (invisible-p (point))))))))))

(ert-deftest codex-ide-status-reopening-buffer-does-not-duplicate-content ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Stored preview")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status)
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(let ((content (buffer-string)))
					  (should (= (how-many "Stored preview"
							       (point-min)
							       (point-max))
						     1))
					  (should (= (how-many "Thread ID"
							       (point-min)
							       (point-max))
						     1))
					  (should (string-match-p "Stored preview" content))))))))))

(ert-deftest codex-ide-status-stripes-every-other-session-heading ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-1")
                     (preview . "First preview")
                     (createdAt . 10)
                     (updatedAt . 30))
                    ((id . "thread-2")
                     (preview . "Second preview")
                     (createdAt . 20)
                     (updatedAt . 20))
                    ((id . "thread-3")
                     (preview . "Third preview")
                     (createdAt . 30)
                     (updatedAt . 10)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(dolist (entry '(("First preview" nil)
							 ("Second preview" t)
							 ("Third preview" nil)))
					  (goto-char (point-min))
					  (search-forward (car entry))
					  (should
					   (eq (codex-ide-status-mode-test--face-includes-p
						(get-text-property (match-beginning 0) 'face)
						'codex-ide-status-striped-heading-face)
					       (cadr entry)))))))))))

(ert-deftest codex-ide-status-cold-start-creates-query-only-session ()
  (let ((project-dir (codex-ide-test--make-temp-project))
        (requests nil))
    (codex-ide-test-with-fixture project-dir
				 (codex-ide-test-with-fake-processes
				  (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					     (lambda () nil))
					    ((symbol-function 'codex-ide--request-sync)
					     (lambda (_session method _params)
					       (push method requests)
					       (pcase method
						 ("initialize" '((ok . t)))
						 (_ (ert-fail (format "Unexpected method %s" method))))))
					    ((symbol-function 'codex-ide--list-threads-page)
					     (lambda (_session &rest _args) '((data . [])))))
				    (let ((default-directory project-dir))
				      (codex-ide-status))
				    (should (equal (seq-remove (lambda (method)
								 (equal method "config/read"))
							       (nreverse requests))
						   '("initialize")))
				    (should (= (length codex-ide--sessions) 1))
				    (let ((session (car codex-ide--sessions)))
				      (should (codex-ide-session-query-only session))
				      (should-not (codex-ide-session-buffer session))
				      (should (buffer-live-p (codex-ide-test--log-buffer session)))))))))

(ert-deftest codex-ide-status-keeps-all-sibling-headings-visible-when-entries-are-collapsed ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20))
                    ((id . "thread-beta")
                     (preview . "Beta thread")
                     (createdAt . 30)
                     (updatedAt . 40)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session-a nil)
					(session-b nil))
				    (let ((default-directory project-dir))
				      (setq session-a (codex-ide--create-process-session))
				      (setq session-b (codex-ide--create-process-session)))
				    (codex-ide-status-mode-test--set-session-buffer-prompts
				     session-a
				     '("First prompt"))
				    (codex-ide-status-mode-test--set-session-buffer-prompts
				     session-b
				     '("Second prompt"))
				    (setf (codex-ide-session-thread-id session-a) "thread-alpha"
					  (codex-ide-session-thread-id session-b) "thread-beta"
					  (codex-ide-session-status session-a) "idle"
					  (codex-ide-session-status session-b) "running")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session-a))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(should-not (invisible-p (point)))
					(search-forward "Second prompt")
					(beginning-of-line)
					(should-not (invisible-p (point))))))))))

(ert-deftest codex-ide-status-heading-previews-use-only-first-line ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (long-preview "This is a deliberately long preview line that should be truncated in the heading only")
         (threads `(((id . "thread-alpha")
                     (preview . ,long-preview)
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (codex-ide-status-mode-test--set-session-buffer-prompts
				     session
				     (list long-preview))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(let ((preview (codex-ide-status-mode--preview-line long-preview)))
					  (goto-char (point-min))
					  (search-forward preview)
					  (should-not (eq (get-text-property (match-beginning 0) 'face)
							  'font-lock-doc-face))
					  (should-not (string-match-p "\n" preview))
					(should (string-match-p (regexp-quote long-preview)
								  (buffer-string)))))))))))

(ert-deftest codex-ide-status-preview-line-trims-multiline-preview ()
  (should (equal (codex-ide-status-mode--preview-line
                  "First preview line\nSecond preview line")
                 "First preview line")))

(ert-deftest codex-ide-status-all-heading-fits-visible-width ()
  (with-temp-buffer
    (codex-ide-status-mode)
    (setq-local codex-ide-status-mode--scope 'all)
    (cl-letf (((symbol-function 'codex-ide-status-mode--display-width)
               (lambda () 24)))
      (let ((heading
             (codex-ide-status-mode--fit-heading
              "Stored  just now  A deliberately long preview")))
        (should (<= (string-width heading) 23))
        (should-not (string-match-p "\n" heading))
        (should (string-suffix-p "…" heading))))))

(ert-deftest codex-ide-status-buffer-heading-uses-first-submitted-prompt-text ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (codex-ide-status-mode-test--set-session-buffer-prompts
				     session
				     '("First prompt" "Submitted prompt")
				     "I am the real active prompt")
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "First prompt")
					(let ((line-end (line-end-position)))
					  (goto-char (line-beginning-position))
					  (should-not (search-forward "Submitted prompt" line-end t))
					  (goto-char (line-beginning-position))
					  (should-not (search-forward "I am the real active prompt" line-end t))))))))))

(ert-deftest codex-ide-status-buffer-expanded-view-shows-last-prompt-and-last-response ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (codex-ide-status-mode-test--set-session-buffer-with-response
				     session
				     '("First prompt"
				       "Explain\nfailure")
				     '("Assistant reply"))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "First prompt")
					(beginning-of-line)
					(codex-ide-section-toggle-at-point)
					(let ((buffer-section-end (point-max)))
					  (should (search-forward "* Buffer:" buffer-section-end t))
					  (should (search-forward (buffer-name (codex-ide-session-buffer session))
								  buffer-section-end
								  t))
					  (should (button-at (match-beginning 0)))
					  (should (search-forward "* Number of Prompts: 2" buffer-section-end t))
					  (should (search-forward "* Last Prompt: Explain\nfailure" buffer-section-end t))
					  (should (codex-ide-status-mode-test--face-includes-p
						   (get-text-property (match-beginning 0) 'face)
						   'codex-ide-status-expanded-content-face))
					  (should (search-forward "* Last Response: Assistant reply" buffer-section-end t))
					  (should (codex-ide-status-mode-test--face-includes-p
						   (get-text-property (match-beginning 0) 'face)
						   'codex-ide-status-expanded-content-face))))))))))

(ert-deftest codex-ide-status-current-turn-range-is-nil-before-output ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir)))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (with-current-buffer (codex-ide-session-buffer session)
				      (let ((inhibit-read-only t)
					    (start nil))
					(erase-buffer)
					(setq start (point))
					(insert "> Waiting for first output")
					(codex-ide--style-user-prompt-region start (point))
					(insert "\n\n"))
				      (codex-ide--insert-input-prompt session ""))
				    (setf (codex-ide-session-status session) "running")
				    (should-not
				     (codex-ide-status-mode--current-turn-transcript-range
				      session))
				    (should-not
				     (codex-ide-status-mode--buffer-transcript-slice
				      session)))))))

(ert-deftest codex-ide-status-thread-expanded-view-shows-buffer-details-when-linked ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Stored preview")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (codex-ide-status-mode-test--set-session-buffer-with-response
				     session
				     '("First prompt"
				       "Explain\nfailure")
				     '("Assistant reply"))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(let ((line-end (line-end-position)))
					  (should (search-forward "First prompt" line-end t)))
					(codex-ide-section-toggle-at-point)
					(forward-line 1)
					(should (looking-at-p (regexp-quote "* Thread ID: thread-alpha")))
					(should (search-forward
						 (format "* Created: %s"
							 (codex-ide--format-thread-updated-at 10))
						 nil t))
					(should (search-forward
						 (format "* Updated: %s"
							 (codex-ide--format-thread-updated-at 20))
						 nil t))
					(should (search-forward "* Buffer:" nil t))
					(should (search-forward (buffer-name (codex-ide-session-buffer session)) nil t))
					(should (button-at (match-beginning 0)))
					(should (search-forward "* Number of Prompts: 2" nil t))
					(should (search-forward "* Last Prompt: Explain\nfailure" nil t))
					(should (search-forward "* Last Response: Assistant reply" nil t)))))))))

(ert-deftest codex-ide-status-thread-expanded-view-omits-buffer-details-when-unlinked ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-stored")
                     (preview . "Stored preview")
                     (createdAt . 30)
                     (updatedAt . 40)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "Stored preview")
					(beginning-of-line)
					(codex-ide-section-toggle-at-point)
					(let ((thread-text
					       (buffer-substring-no-properties
						(line-beginning-position 2)
						(save-excursion
						  (forward-line 4)
						  (line-beginning-position)))))
					  (should (string-match-p "\\* Thread ID: thread-stored" thread-text))
					  (should (string-match-p
						   (regexp-quote
						    (format "* Created: %s"
							    (codex-ide--format-thread-updated-at 30)))
						   thread-text))
					  (should (string-match-p
						   (regexp-quote
						    (format "* Updated: %s"
							    (codex-ide--format-thread-updated-at 40)))
						   thread-text))
					  (should-not (string-match-p "Number of Prompts:" thread-text))
					  (should-not (string-match-p "Buffer:" thread-text))
					  (should-not (string-match-p "Last Prompt:" thread-text))
					  (should-not (string-match-p "Last Response:" thread-text))))))))))

(ert-deftest codex-ide-status-tab-toggles-session-section-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "Alpha thread")
					(beginning-of-line)
					(should-not (invisible-p (point)))
					(forward-line 1)
					(should (invisible-p (point)))
					(forward-line -1)
					(codex-ide-section-toggle-at-point)
					(forward-line 1)
					(should-not (invisible-p (point)))
					(forward-line -1)
					(codex-ide-section-toggle-at-point)
					(forward-line 1)
					(should (invisible-p (point))))))))))

(ert-deftest codex-ide-status-plus-starts-a-new-session ()
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "+"))
              #'codex-ide)))

(ert-deftest codex-ide-status-mode-exposes-focal-point-navigation-commands ()
  (should (commandp #'codex-ide-status-mode-nav-forward))
  (should (commandp #'codex-ide-status-mode-nav-backward)))

(ert-deftest codex-ide-status-refresh-preserves-expanded-sections ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (codex-ide-status-mode-test--set-session-buffer-with-response
				     session
				     '("Submitted prompt")
				     '("Assistant reply"))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "Submitted prompt")
					(beginning-of-line)
					(codex-ide-section-toggle-at-point)
					(codex-ide-status-mode-refresh)
					(goto-char (point-min))
					(search-forward "Submitted prompt")
					(beginning-of-line)
					(forward-line 1)
					(should-not (invisible-p (point))))))))))

(ert-deftest codex-ide-status-refresh-preserves-point-relative-to-section ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil)
					(expected-offset nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (codex-ide-status-mode-test--set-session-buffer-with-response
				     session
				     '("Submitted prompt")
				     '("first line" "second line"))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "Submitted prompt")
					(beginning-of-line)
					(codex-ide-section-toggle-at-point)
					(search-forward "* Last Response: first line\nsecond line")
					(goto-char (match-beginning 0))
					(setq expected-offset
					      (- (point)
						 (codex-ide-section-heading-start
						  (codex-ide-section-containing-point))))
					(codex-ide-status-mode-refresh)
					(should (equal (codex-ide-section-value
							(codex-ide-section-containing-point))
						       (car threads)))
					(should (= (- (point)
						      (codex-ide-section-heading-start
						       (codex-ide-section-containing-point)))
						   expected-offset))
					(should (string-match-p
						 "Last Response: first line\nsecond line"
						 (buffer-substring-no-properties
						  (codex-ide-section-heading-start
						   (codex-ide-section-containing-point))
						  (codex-ide-section-end
						   (codex-ide-section-containing-point))))))))))))

(ert-deftest codex-ide-status-refresh-keeps-existing-render-when-prepare-fails ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads `(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(let ((original-body (buffer-string))
					      (original-header (codex-ide-status-mode-test--header-line-string)))
					  (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
						     (lambda ()
						       (user-error "Preparation failed"))))
					    (should-error (codex-ide-status-mode-refresh) :type 'user-error))
					  (should (equal (buffer-string) original-body))
					  (should (equal (codex-ide-status-mode-test--header-line-string)
							 original-header))))))))))

(ert-deftest codex-ide-status-ret-visits-thread-section-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20))))
         (requests nil)
         (thread-read
          '((thread . ((id . "thread-alpha")))
            (turns . (((id . "turn-1")
                       (items . (((type . "userMessage")
                                  (content . (((type . "text")
                                               (text . "Resume from status")))))
                                 ((type . "agentMessage")
                                  (id . "item-1")
                                  (text . "Resumed from status mode."))))))))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil)
					(prepare-count 0))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session))
				      (codex-ide--set-session-status session "idle"))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda ()
						 (setq prepare-count (1+ prepare-count))))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads))))
					      ((symbol-function 'codex-ide--request-sync)
					       (lambda (_session method _params)
						 (push method requests)
						 (pcase method
						   ("initialize" '((ok . t)))
						   ("thread/read" thread-read)
						   ("thread/resume" '((ok . t)))
						   (_ (ert-fail (format "Unexpected method %s" method))))))
					      ((symbol-function 'codex-ide-display-buffer)
					       (lambda (_buffer &optional _action)
						 (selected-window))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "thread-a")
					(beginning-of-line)
					(call-interactively #'codex-ide-status-mode-display-session-at-point)
					(should (= prepare-count 2))
					(should (equal (seq-remove (lambda (method)
								     (equal method "initialize"))
								   (nreverse requests))
						       '("thread/read" "thread/resume")))
					(should (equal (codex-ide-session-thread-id session)
						       "thread-alpha"))
					(with-current-buffer (codex-ide-session-buffer session)
					  (should (string-match-p "^> Resume from status" (buffer-string)))
					  (should (string-match-p "Resumed from status mode\\."
								  (buffer-string)))))))))))

(ert-deftest codex-ide-status-delete-removes-thread-section-at-point ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil)
					(deleted-thread-id nil)
					(skip-confirmation nil)
					(prepare-count 0))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda ()
						 (setq prepare-count (1+ prepare-count))))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
						 (list
						  (cons 'data
							(if deleted-thread-id nil threads)))))
					      ((symbol-function 'codex-ide-delete-session-thread)
					       (lambda (thread-id &optional skip-confirm)
						 (setq deleted-thread-id thread-id
						       skip-confirmation skip-confirm)))
					      ((symbol-function 'y-or-n-p)
					       (lambda (&rest _)
						 (ert-fail "Unexpected y-or-n-p prompt")))
					      ((symbol-function 'yes-or-no-p)
					       (lambda (prompt)
						 (should (equal prompt
								(format "Permanently remove 1 Codex thread from %s? "
									(abbreviate-file-name
									 (codex-ide--codex-home)))))
						 t)))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "thread-a")
					(beginning-of-line)
					(call-interactively #'codex-ide-status-mode-delete-thing-at-point)
					(should (equal deleted-thread-id "thread-alpha"))
					(should skip-confirmation)
					(should (= prepare-count 3))
					(should (equal (codex-ide-status-mode-test--header-line-string)
						       "Project: alpha | 0 sessions")))))))))

(ert-deftest codex-ide-status-delete-removes-thread-sections-in-region ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20))
                    ((id . "thread-beta")
                     (preview . "Beta thread")
                     (createdAt . 30)
                     (updatedAt . 40))
                    ((id . "thread-gamma")
                     (preview . "Gamma thread")
                     (createdAt . 50)
                     (updatedAt . 60)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil)
					(deleted-thread-ids nil)
					(skip-confirmations nil)
					(prepare-count 0)
					(confirmation-count 0))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda ()
						 (setq prepare-count (1+ prepare-count))))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
						 (list
						  (cons
						   'data
						   (seq-remove
						    (lambda (thread)
						      (member (alist-get 'id thread)
							      deleted-thread-ids))
						    threads)))))
					      ((symbol-function 'codex-ide-delete-session-thread)
					       (lambda (thread-id &optional skip-confirm)
						 (setq deleted-thread-ids
						       (append deleted-thread-ids (list thread-id)))
						 (setq skip-confirmations
						       (append skip-confirmations (list skip-confirm)))))
					      ((symbol-function 'y-or-n-p)
					       (lambda (&rest _)
						 (ert-fail "Unexpected y-or-n-p prompt")))
					      ((symbol-function 'yes-or-no-p)
					       (lambda (prompt)
						 (setq confirmation-count (1+ confirmation-count))
						 (should (equal prompt
								(format "Permanently remove 2 Codex threads from %s? "
									(abbreviate-file-name
									 (codex-ide--codex-home)))))
						 t)))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "Beta thread")
					(beginning-of-line)
					(push-mark (save-excursion
						     (search-forward "Alpha thread")
						     (line-end-position))
						   t
						   t)
					(activate-mark)
					(call-interactively #'codex-ide-status-mode-delete-thing-at-point)
					(should (= confirmation-count 1))
					(should (equal (sort (copy-sequence deleted-thread-ids) #'string-lessp)
						       '("thread-alpha" "thread-beta")))
					(should (equal skip-confirmations '(t t)))
					(should (= prepare-count 3))
					(should (equal (codex-ide-status-mode-test--header-line-string)
						       "Project: alpha | 1 session"))
					(should-not (string-match-p "thread-a" (buffer-string)))
					(should-not (string-match-p "thread-b" (buffer-string)))
					(should (string-match-p "thread-g" (buffer-string))))))))))

(ert-deftest codex-ide-status-kill-buffer-at-point-kills-live-thread-buffer ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session))
				      (setf (codex-ide-session-thread-id session) "thread-alpha"))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads))))
					      ((symbol-function 'y-or-n-p)
					       (lambda (prompt)
						 (should (equal prompt
								(format "Kill Codex session buffer %s? "
									(buffer-name
									 (codex-ide-session-buffer session)))))
						 t)))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "thread-a")
					(beginning-of-line)
					(call-interactively #'codex-ide-status-mode-kill-buffer-at-point)
					(should-not (buffer-live-p (codex-ide-session-buffer session)))
					(should (string-match-p "Stored  .*  Alpha thread" (buffer-string)))
					(should (equal (codex-ide-status-mode-test--header-line-string)
						       "Project: alpha | 1 session")))))))))

(ert-deftest codex-ide-status-kill-buffer-at-point-errors-without-associated-buffer ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "thread-a")
					(beginning-of-line)
					(let ((err (should-error
						    (call-interactively
						     #'codex-ide-status-mode-kill-buffer-at-point)
						    :type 'user-error)))
					  (should (equal (cadr err)
							 "No buffer for this session."))))))))))

(ert-deftest codex-ide-status-kill-buffer-at-point-region-ignores-stored-threads ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads '(((id . "thread-alpha")
                     (preview . "Alpha thread")
                     (createdAt . 10)
                     (updatedAt . 20))
                    ((id . "thread-beta")
                     (preview . "Beta thread")
                     (createdAt . 30)
                     (updatedAt . 40)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil)
					(confirmation-count 0))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session))
				      (setf (codex-ide-session-thread-id session) "thread-alpha"))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads))))
					      ((symbol-function 'y-or-n-p)
					       (lambda (prompt)
						 (setq confirmation-count (1+ confirmation-count))
						 (should (equal prompt
								(format "Kill Codex session buffer %s? "
									(buffer-name
									 (codex-ide-session-buffer session)))))
						 t)))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward "Beta thread")
					(beginning-of-line)
					(push-mark (save-excursion
						     (search-forward "Alpha thread")
						     (line-end-position))
						   t
						   t)
					(activate-mark)
					(call-interactively #'codex-ide-status-mode-kill-buffer-at-point)
					(should (= confirmation-count 1))
					(should-not (buffer-live-p (codex-ide-session-buffer session)))
					(should (string-match-p "Stored  .*  Alpha thread" (buffer-string)))
					(should (string-match-p "Stored  .*  Beta thread" (buffer-string)))
					(should (equal (codex-ide-status-mode-test--header-line-string)
						       "Project: alpha | 2 sessions")))))))))

(ert-deftest codex-ide-status-actions-reject-parent-sections ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (threads nil))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(should (equal (codex-ide-status-mode-test--header-line-string)
						       "Project: alpha | 0 sessions"))
					(should-error
					 (call-interactively #'codex-ide-status-mode-display-session-at-point)
					 :type 'user-error)
					(should-error
					 (call-interactively #'codex-ide-status-mode-delete-thing-at-point)
					 :type 'user-error))))))))

(ert-deftest codex-ide-status-mode-binds-session-display-commands ()
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "RET"))
              #'codex-ide-status-mode-display-session-at-point))
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "M-<return>"))
              #'codex-ide-status-mode-display-session-at-point-other-window))
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "C-M-j"))
              #'codex-ide-status-mode-display-session-at-point-other-window)))

(ert-deftest codex-ide-status-mode-binds-kill-buffer-command ()
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "K"))
              #'codex-ide-status-mode-kill-buffer-at-point)))

(ert-deftest codex-ide-status-display-session-at-point-other-window-binds-pop-up-action ()
  (let ((captured-action nil)
        (section 'buffer-section))
    (with-temp-buffer
      (codex-ide-status-mode)
      (cl-letf (((symbol-function 'codex-ide-status-mode--actionable-section-at-point)
                 (lambda () section))
                ((symbol-function 'codex-ide-status-mode--visit-section)
                 (lambda (_section)
                   (setq captured-action codex-ide-display-buffer-pop-up-action))))
        (call-interactively
         #'codex-ide-status-mode-display-session-at-point-other-window)))
    (should (equal captured-action
                   '((display-buffer-reuse-window
                      display-buffer-use-some-window
                      display-buffer-pop-up-window)
                     (inhibit-same-window . t))))))

(ert-deftest codex-ide-status-thread-section-hides-context-preview-and-shows-compact-metadata ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (thread-preview (concat "[Emacs prompt context]\n"
                                 "Buffer: sample.el\n"
                                 "[/Emacs prompt context]\n\n"
                                 "Thread preview first line\n"
                                 "Thread preview second line"))
         (threads `(((id . "thread-alpha")
                     (preview . ,thread-preview)
                     (createdAt . 10)
                     (updatedAt . 20)))))
    (make-directory project-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (setf (codex-ide-session-thread-id session) "thread-alpha"
					  (codex-ide-session-status session) "idle")
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
                                                 (list (cons 'data threads)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(goto-char (point-min))
					(search-forward
					 (codex-ide-status-mode--preview-line
					  (codex-ide--thread-choice-preview thread-preview)))
					(beginning-of-line)
					(codex-ide-section-toggle-at-point)
					(should (search-forward "* Thread ID: thread-alpha" nil t))
					(should (codex-ide-status-mode-test--face-includes-p
						 (get-text-property (match-beginning 0) 'face)
						 'codex-ide-status-expanded-content-face))
					(should (search-forward
						 (format "* Created: %s"
							 (codex-ide--format-thread-updated-at 10))
						 nil t))
					(should (codex-ide-status-mode-test--face-includes-p
						 (get-text-property (match-beginning 0) 'face)
						 'codex-ide-status-expanded-content-face))
					(should (search-forward
						 (format "* Updated: %s"
							 (codex-ide--format-thread-updated-at 20))
						 nil t))
					(should (codex-ide-status-mode-test--face-includes-p
						 (get-text-property (match-beginning 0) 'face)
						 'codex-ide-status-expanded-content-face))
					(should-not (search-forward "Last Prompt:" nil t))
					(should-not (search-forward "Last Response:" nil t))
					(should-not (search-forward "Thread preview first line" nil t))
					(should-not (search-forward "Thread preview second line" nil t))
					(should-not (search-forward "Buffer: sample.el" nil t)))))))))

(ert-deftest codex-ide-status-auto-refresh-subscribes-and-filters-session-events ()
  (let* ((root-dir (codex-ide-test--make-temp-project))
         (project-dir (expand-file-name "alpha" root-dir))
         (other-dir (expand-file-name "beta" root-dir))
         (scheduled-callback nil)
         (scheduled-args nil)
         (refresh-count 0))
    (make-directory project-dir t)
    (make-directory other-dir t)
    (codex-ide-test-with-fixture root-dir
				 (codex-ide-test-with-fake-processes
				  (let ((session nil)
					(other-session nil))
				    (let ((default-directory project-dir))
				      (setq session (codex-ide--create-process-session)))
				    (let ((default-directory other-dir))
				      (setq other-session (codex-ide--create-process-session)))
				    (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
					       (lambda () nil))
					      ((symbol-function 'codex-ide--ensure-query-session-for-thread-selection)
					       (lambda (_directory) session))
					      ((symbol-function 'codex-ide--list-threads-page)
					       (lambda (_session &rest _args)
						 '((data . []))))
					      ((symbol-function 'run-with-idle-timer)
					       (lambda (_delay _repeat function &rest args)
						 (setq scheduled-callback function
						       scheduled-args args)
						 'codex-ide-status-test-timer))
					      ((symbol-function 'timerp)
					       (lambda (object)
						 (eq object 'codex-ide-status-test-timer)))
					      ((symbol-function 'cancel-timer)
					       (lambda (_timer)
						 nil))
					      ((symbol-function 'codex-ide-renderer-schedule-theme-refresh)
					       (lambda ()
						 nil))
					      ((symbol-function 'codex-ide-status-mode--schedule-theme-refresh)
					       (lambda ()
						 nil))
					      ((symbol-function 'codex-ide-status-mode-refresh)
					       (lambda (&optional _ignore-auto _noconfirm)
						 (setq refresh-count (1+ refresh-count)))))
				      (let ((default-directory project-dir))
					(codex-ide-status))
				      (with-current-buffer "codex-ide: alpha"
					(run-hook-with-args 'codex-ide-session-event-hook
							    'status-changed
							    other-session
							    '(:status "running"))
					(should-not scheduled-callback)
					(run-hook-with-args 'codex-ide-session-event-hook
							    'status-changed
							    session
							    '(:status "running"))
					(should (eq scheduled-callback
						    #'codex-ide-status-mode--run-scheduled-refresh))
					(should (equal scheduled-args (list (current-buffer))))
					(run-hook-with-args 'codex-ide-session-event-hook
							    'status-changed
							    session
							    '(:status "idle"))
					(should (= refresh-count 0))
					(apply scheduled-callback scheduled-args)
					(should (= refresh-count 1))
					(should-not codex-ide-status-mode--refresh-timer)
					(let ((listener codex-ide-status-mode--event-listener))
					  (kill-buffer (current-buffer))
					  (should-not (member listener codex-ide-session-event-hook))))))))))

(ert-deftest codex-ide-status-theme-refresh-subscribes-and-tears-down-hooks ()
  (let ((renderer-schedule-count 0)
        (status-schedule-count 0)
        (codex-ide-status-mode--theme-refresh-buffers nil)
        (buffer (generate-new-buffer " *codex-ide-status-theme-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'codex-ide-renderer-schedule-theme-refresh)
                     (lambda ()
                       (setq renderer-schedule-count
                             (1+ renderer-schedule-count))))
                    ((symbol-function 'codex-ide-status-mode--schedule-theme-refresh)
                     (lambda ()
                       (setq status-schedule-count
                             (1+ status-schedule-count)))))
            (codex-ide-status-mode)
            (should (memq (current-buffer) codex-ide-status-mode--theme-refresh-buffers))
            (should (memq #'codex-ide-status-mode--handle-theme-change
                          enable-theme-functions))
            (should (memq #'codex-ide-status-mode--handle-theme-change
                          disable-theme-functions))
            (should (= renderer-schedule-count 1))
            (should (= status-schedule-count 1))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (run-hook-with-args 'disable-theme-functions 'test-theme)
            (should (= renderer-schedule-count 3))
            (should (= status-schedule-count 3))
            (fundamental-mode)
            (should-not (memq (current-buffer) codex-ide-status-mode--theme-refresh-buffers))
            (should-not (memq #'codex-ide-status-mode--handle-theme-change
                              enable-theme-functions))
            (should-not (memq #'codex-ide-status-mode--handle-theme-change
                              disable-theme-functions))
            (codex-ide-status-mode)
            (should (memq (current-buffer) codex-ide-status-mode--theme-refresh-buffers))
            (should (memq #'codex-ide-status-mode--handle-theme-change
                          enable-theme-functions))
            (should (memq #'codex-ide-status-mode--handle-theme-change
                          disable-theme-functions))
            (should (= renderer-schedule-count 4))
            (should (= status-schedule-count 4))
            (run-hook-with-args 'enable-theme-functions 'test-theme)
            (should (= renderer-schedule-count 5))
            (should (= status-schedule-count 5))
            (kill-buffer buffer)
            (should-not (memq buffer codex-ide-status-mode--theme-refresh-buffers))
            (should-not (memq #'codex-ide-status-mode--handle-theme-change
                              enable-theme-functions))
            (should-not (memq #'codex-ide-status-mode--handle-theme-change
                              disable-theme-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-status-theme-refresh-defers-and-coalesces ()
  (let ((codex-ide-status-mode--theme-refresh-timer nil)
        (codex-ide-status-mode--theme-refresh-buffers nil)
        (scheduled-count 0)
        (refresh-count 0)
        scheduled-callback
        (buffer (generate-new-buffer " *codex-ide-status-theme-timer-test*")))
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_secs _repeat function &rest _args)
                     (setq scheduled-count (1+ scheduled-count)
                           scheduled-callback function)
                     :timer))
                  ((symbol-function 'codex-ide-status-mode--refresh-striped-heading-face)
                   (lambda ()
                     (setq refresh-count (1+ refresh-count)))))
          (setq codex-ide-status-mode--theme-refresh-buffers (list buffer))
          (codex-ide-status-mode--schedule-theme-refresh)
          (codex-ide-status-mode--schedule-theme-refresh)
          (should (= scheduled-count 1))
          (should (= refresh-count 0))
          (funcall scheduled-callback)
          (should (= refresh-count 1))
          (should-not codex-ide-status-mode--theme-refresh-timer)
          (codex-ide-status-mode--schedule-theme-refresh)
          (should (= scheduled-count 2)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest codex-ide-status-all-binds-filter-refresh-and-quit-commands ()
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "/"))
              #'codex-ide-status-mode-filter))
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "s"))
              #'codex-ide-status-mode-filter))
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "C-g"))
              #'codex-ide-status-mode-quit))
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "q"))
              #'codex-ide-status-mode-quit))
  (should (eq (lookup-key codex-ide-status-mode-map (kbd "g r"))
              #'codex-ide-status-mode-refresh)))

(ert-deftest codex-ide-status-filter-searches-preview-name-directory-and-id ()
  (let ((thread '((id . "thread-needle")
                  (name . "Named session")
                  (cwd . "/tmp/directory-needle")
                  (preview . "Human preview needle"))))
    (with-temp-buffer
      (codex-ide-status-mode)
      (setq-local codex-ide-status-mode--threads (list thread))
      (dolist (filter '("human preview" "named session"
                        "directory-needle" "thread-needle"))
        (setq codex-ide-status-mode--filter filter)
        (should (codex-ide-status-mode--filter-match-p thread)))
      (setq codex-ide-status-mode--filter "unrelated")
      (should-not (codex-ide-status-mode--filter-match-p thread)))))

(ert-deftest codex-ide-status-filter-searches-live-last-prompt-and-response ()
  (let ((thread '((id . "thread-live")
                  (preview . "Initial prompt")))
        (session (make-codex-ide-session)))
    (with-temp-buffer
      (codex-ide-status-mode)
      (cl-letf (((symbol-function 'codex-ide-status-mode--thread-session)
                 (lambda (_thread _directory) session))
                ((symbol-function
                  'codex-ide-status-mode--last-submitted-prompt-text)
                 (lambda (_session) "Latest multiline\nprompt"))
                ((symbol-function 'codex-ide-status-mode--buffer-transcript-slice)
                 (lambda (_session) "Latest multiline\nresponse")))
        (setq codex-ide-status-mode--filter "multiline\nprompt")
        (should (codex-ide-status-mode--filter-match-p thread))
        (setq codex-ide-status-mode--filter "multiline\nresponse")
        (should (codex-ide-status-mode--filter-match-p thread))))))

(ert-deftest codex-ide-status-quit-clears-filter-before-closing ()
  (let ((rendered nil)
        (quit nil))
    (with-temp-buffer
      (codex-ide-status-mode)
      (setq-local codex-ide-status-mode--directory temporary-file-directory
                  codex-ide-status-mode--filter "needle")
      (cl-letf (((symbol-function 'codex-ide-status-mode--render-buffer)
                 (lambda (&rest _args) (setq rendered t)))
                ((symbol-function 'quit-window)
                 (lambda (&rest _args) (setq quit t))))
        (codex-ide-status-mode-quit)
        (should rendered)
        (should-not quit)
        (should (string-empty-p codex-ide-status-mode--filter))
        (codex-ide-status-mode-quit)
        (should quit)))))

(ert-deftest codex-ide-status-all-renders-and-resumes-with-thread-directory ()
  (let* ((directory "/tmp/status-query")
         (thread-directory "/tmp/original-project")
         (preview
          (concat "[Emacs prompt context]\n"
                  "Buffer: source.el\n"
                  "[/Emacs prompt context]\n\n"
                  "First human message\nSecond human message"))
         (thread `((id . "thread-all")
                   (name . "Persistent name")
                   (cwd . ,thread-directory)
                   (preview . ,preview)
                   (createdAt . 10)
                   (updatedAt . 20)))
         (visited nil))
    (with-temp-buffer
      (codex-ide-status-mode)
      (setq-local codex-ide-status-mode--directory directory
                  codex-ide-status-mode--scope 'all
                  codex-ide-status-mode--threads (list thread))
      (cl-letf (((symbol-function 'codex-ide--prepare-session-operations)
                 (lambda () nil))
                ((symbol-function 'codex-ide--show-or-resume-thread)
                 (lambda (thread-id cwd)
                   (setq visited (list thread-id cwd)))))
        (codex-ide-status-mode--render-buffer
         directory :is-refresh t :reload nil)
        (should (equal (codex-ide-status-mode-test--header-line-string)
                       "All sessions | 1 session"))
        (should (search-forward
                 "Persistent name First human message"
                 nil t))
        (should-not (search-forward "Emacs prompt context" nil t))
        (beginning-of-line)
        (codex-ide-section-toggle-at-point)
        (should (search-forward "First human message\nSecond human message" nil t))
        (should (search-forward "* Thread ID: thread-all" nil t))
        (should (search-forward
                 (format "* Directory: %s"
                         (abbreviate-file-name thread-directory))
                 nil t))
        (goto-char (point-min))
        (search-forward "Persistent name")
        (beginning-of-line)
        (codex-ide-status-mode-display-session-at-point)
        (should (equal visited (list "thread-all" thread-directory)))))))

(ert-deftest codex-ide-status-show-more-appends-next-page ()
  (let ((first '((id . "thread-1") (cwd . "/tmp/one")))
        (second '((id . "thread-2") (cwd . "/tmp/two")))
        (rendered nil))
    (with-temp-buffer
      (codex-ide-status-mode)
      (setq-local codex-ide-status-mode--scope 'project
                  codex-ide-status-mode--directory "/tmp/query"
                  codex-ide-status-mode--threads (list first)
                  codex-ide-status-mode--next-cursor "page-2")
      (cl-letf (((symbol-function 'codex-ide-status-mode--request-page)
                 (lambda (cursor)
                   (should (equal cursor "page-2"))
                   `((data . [,second]) (nextCursor . nil))))
                ((symbol-function 'codex-ide-status-mode--render-buffer)
                 (lambda (&rest _args)
                   (setq rendered t))))
        (codex-ide-status-mode-show-more)
        (should rendered)
        (should (equal (mapcar (lambda (thread) (alist-get 'id thread))
                               codex-ide-status-mode--threads)
                       '("thread-1" "thread-2")))
        (should-not codex-ide-status-mode--next-cursor)))))

(ert-deftest codex-ide-status-show-more-is-a-button-after-sessions ()
  (let ((called nil))
    (with-temp-buffer
      (codex-ide-status-mode)
      (setq-local codex-ide-status-mode--scope 'project
                  codex-ide-status-mode--next-cursor "page-2")
      (cl-letf (((symbol-function 'codex-ide-status-mode-show-more)
                 (lambda () (setq called t))))
        (let ((inhibit-read-only t))
          (codex-ide-status-mode--insert-show-more-button))
        (goto-char (point-min))
        (search-forward "Show more sessions")
        (button-activate (button-at (match-beginning 0)))
        (should called)))))

(ert-deftest codex-ide-status-project-retains-next-page-cursor ()
  (let* ((directory temporary-file-directory)
         (session (make-codex-ide-session
                   :directory directory
                   :process 'query-process))
         (captured nil))
    (with-temp-buffer
      (codex-ide-status-mode)
      (setq-local codex-ide-status-mode--directory directory
                  codex-ide-status-mode--scope 'project
                  codex-ide-status-mode--query-session session)
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (process) (eq process 'query-process)))
                ((symbol-function 'codex-ide--list-threads-page)
                 (lambda (request-session &rest args)
                   (setq captured (cons request-session args))
                   '((data . []) (nextCursor . "page-2")))))
        (codex-ide-status-mode--reload-threads)
        (should (eq (car captured) session))
        (should (equal (cdr captured)
                       (list :all-directories nil
                             :cursor nil
                             :limit codex-ide-thread-list-default-limit
                             :sort-key "updated_at")))
        (should (equal codex-ide-status-mode--next-cursor "page-2"))))))

(ert-deftest codex-ide-status-all-requests-all-directories-first-page ()
  (let* ((directory temporary-file-directory)
         (session (make-codex-ide-session
                   :directory directory
                   :process 'query-process))
         (captured nil)
         (buffer-name "*Codex Sessions*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (unwind-protect
        (cl-letf (((symbol-function 'codex-ide--get-working-directory)
                   (lambda () directory))
                  ((symbol-function 'codex-ide--prepare-session-operations)
                   (lambda () nil))
                  ((symbol-function
                    'codex-ide--ensure-query-session-for-thread-selection)
                   (lambda (_directory) session))
                  ((symbol-function 'process-live-p)
                   (lambda (process) (eq process 'query-process)))
                  ((symbol-function 'codex-ide--list-threads-page)
                   (lambda (request-session &rest args)
                     (setq captured (cons request-session args))
                     '((data . []) (nextCursor . "page-2"))))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _args) buffer)))
          (codex-ide-status-all)
          (should (eq (car captured) session))
          (should (equal (cdr captured)
                         (list :all-directories t
                               :cursor nil
                               :limit codex-ide-thread-list-default-limit
                               :sort-key "updated_at")))
          (with-current-buffer buffer-name
            (should (eq codex-ide-status-mode--scope 'all))
            (should-not truncate-lines)
            (should word-wrap)
            (should (equal (codex-ide-status-mode-test--header-line-string)
                           "All sessions | 0 sessions"))
            (goto-char (point-min))
            (search-forward "Show more sessions")
            (should (button-at (match-beginning 0)))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(provide 'codex-ide-status-mode-tests)

;;; codex-ide-status-mode-tests.el ends here
