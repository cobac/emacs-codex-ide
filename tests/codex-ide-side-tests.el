;;; codex-ide-side-tests.el --- Tests for Codex side conversations -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for `codex-ide-side'.

;;; Code:

(require 'ert)
(require 'codex-ide)
(require 'codex-ide-side)
(require 'codex-ide-test-fixtures)

(ert-deftest codex-ide-side-boundary-marks-inherited-history-reference-only ()
  (let* ((item (codex-ide-side--boundary-item))
         (content (alist-get 'content item))
         (text (alist-get 'text (aref content 0))))
    (should (equal (alist-get 'type item) "message"))
    (should (equal (alist-get 'role item) "user"))
    (should (string-match-p "inherited history" text))
    (should (string-match-p "reference context only" text))
    (should (string-match-p
             "Only messages submitted[[:space:]\n]+after"
             text))
    (should (string-match-p "Do not use sub-agents" text))))

(ert-deftest codex-ide-side-forks-ephemeral-thread-and-injects-boundary ()
  (let* ((codex-ide-model "gpt-test")
         (codex-ide-reasoning-effort "high")
         (codex-ide-fast "on")
         (codex-ide-approval-policy "on-request")
         (codex-ide-sandbox-mode "workspace-write")
         (codex-ide-personality "pragmatic")
         (parent
          (make-codex-ide-session
           :directory "/tmp/project"
           :thread-id "thread-parent"
           :status "running"))
         (side
          (make-codex-ide-session
           :directory "/tmp/project"
           :status "idle"))
         requests)
    (cl-letf (((symbol-function 'codex-ide--request-sync)
               (lambda (_session method params)
                 (push (cons method params) requests)
                 (if (equal method "thread/fork")
                     '((thread . ((id . "thread-side")
                                  (model . "gpt-test"))))
                   '()))))
      (should (equal (codex-ide-side--fork parent side)
                     "thread-side")))
    (setq requests (nreverse requests))
    (let ((fork (cdar requests))
          (inject (cdr (cadr requests))))
      (should (equal (mapcar #'car requests)
                     '("thread/fork" "thread/inject_items")))
      (should (equal (alist-get 'threadId fork) "thread-parent"))
      (should (eq (alist-get 'ephemeral fork) t))
      (should (eq (alist-get 'excludeTurns fork) t))
      (should (equal (alist-get 'cwd fork) "/tmp/project"))
      (should (equal (alist-get 'model fork) "gpt-test"))
      (should (equal (alist-get 'serviceTier fork) "priority"))
      (should (equal (alist-get 'threadId inject) "thread-side"))
      (should (= (length (alist-get 'items inject)) 1))
      (should
       (string-match-p
        "Side conversation boundary"
        (alist-get
         'text
         (aref
          (alist-get
           'content
           (aref (alist-get 'items inject) 0))
          0)))))
    (should (equal (codex-ide-session-thread-id side) "thread-side"))
    (should (codex-ide--session-metadata-get side :session-context-sent))))

(ert-deftest codex-ide-side-display-uses-ordinary-pop-to-buffer ()
  (let* ((buffer (generate-new-buffer " *codex-side-display*"))
         (side (make-codex-ide-session :buffer buffer))
         pop-arguments
         ensured)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (&rest arguments)
                     (setq pop-arguments arguments)
                     buffer))
                  ((symbol-function 'codex-ide--ensure-input-prompt)
                   (lambda (session)
                     (setq ensured session))))
          (should (eq (codex-ide-side--display side) side))
          (should (equal pop-arguments (list buffer)))
          (should (eq ensured side)))
      (kill-buffer buffer))))

(ert-deftest codex-ide-side-header-identifies-parent-and-return-action ()
  (let* ((parent-buffer (generate-new-buffer " *codex-parent*"))
         (side-buffer (generate-new-buffer " *codex-side*"))
         (parent
          (make-codex-ide-session
           :buffer parent-buffer
           :status "running"))
         (side
          (make-codex-ide-session
           :buffer side-buffer
           :status "idle")))
    (unwind-protect
        (progn
          (codex-ide--session-metadata-put side :side-parent parent)
          (let ((summary (codex-ide-side--header-summary side)))
            (should (string-match-p (regexp-quote (buffer-name parent-buffer))
                                    summary))
            (should (string-match-p "Running" summary))
            (should (string-match-p "Return to parent" summary))
            (should
             (eq (get-text-property
                  (string-match "Return to parent" summary)
                  'local-map
                  summary)
                 codex-ide-side-return-header-map))))
      (kill-buffer parent-buffer)
      (kill-buffer side-buffer))))

(ert-deftest codex-ide-side-cleanup-interrupts-unsubscribes-and-unlinks-parent ()
  (let* ((parent (make-codex-ide-session :status "running"))
         (side
          (make-codex-ide-session
           :process 'side-process
           :thread-id "thread-side"
           :current-turn-id "turn-side"
           :status "running"))
         requests)
    (codex-ide--session-metadata-put side :side-parent parent)
    (codex-ide--session-metadata-put parent :side-session side)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (process)
                 (eq process 'side-process)))
              ((symbol-function 'codex-ide--request-sync)
               (lambda (_session method params)
                 (push (cons method params) requests)
                 '())))
      (codex-ide-side--request-cleanup side))
    (setq requests (nreverse requests))
    (should (equal (mapcar #'car requests)
                   '("turn/interrupt" "thread/unsubscribe")))
    (should (equal (alist-get 'turnId (cdar requests)) "turn-side"))
    (should (equal (alist-get 'threadId (cdr (cadr requests)))
                   "thread-side"))
    (should-not (codex-ide--session-metadata-get parent :side-session))))

(ert-deftest codex-ide-side-start-cleans-up-when-initialization-or-fork-quits ()
  (dolist (phase '(initialize fork))
    (let ((project-dir (codex-ide-test--make-temp-project)))
      (codex-ide-test-with-fixture project-dir
        (codex-ide-test-with-fake-processes
          (let* ((parent
                  (make-codex-ide-session
                   :directory project-dir
                   :thread-id "thread-parent"
                   :status "idle"))
                 (create-session
                  (symbol-function 'codex-ide--create-process-session))
                 created-side)
            (cl-letf
                (((symbol-function 'codex-ide--create-process-session)
                  (lambda (&rest args)
                    (setq created-side (apply create-session args))))
                 ((symbol-function 'codex-ide--initialize-session)
                  (lambda (_side)
                    (when (eq phase 'initialize)
                      (signal 'quit nil))))
                 ((symbol-function 'codex-ide-side--fork)
                  (lambda (_parent _side)
                    (signal 'quit nil))))
              (let (quit-signaled)
                (condition-case nil
                    (codex-ide-side--start parent nil)
                  (quit
                   (setq quit-signaled t)))
                (should quit-signaled)))
            (should created-side)
            (should-not
             (process-live-p (codex-ide-session-process created-side)))
            (should-not
             (buffer-live-p (codex-ide-session-buffer created-side)))
            (should-not (memq created-side codex-ide--sessions))
            (should-not
             (codex-ide--session-metadata-get parent :side-session))))))))

(ert-deftest codex-ide-default-session-selection-excludes-side-conversations ()
  (let* ((directory (codex-ide--normalize-directory "/tmp/project"))
         (parent
          (make-codex-ide-session
           :directory directory
           :process 'parent-process
           :created-at 1))
         (side
          (make-codex-ide-session
           :directory directory
           :process 'side-process
           :created-at 2))
         (codex-ide--sessions (list side parent)))
    (codex-ide--session-metadata-put side :side-parent parent)
    (cl-letf (((symbol-function 'process-live-p)
               (lambda (process)
                 (memq process '(parent-process side-process)))))
      (should (eq (codex-ide--last-active-session-for-directory
                   directory)
                  parent)))))

(provide 'codex-ide-side-tests)

;;; codex-ide-side-tests.el ends here
