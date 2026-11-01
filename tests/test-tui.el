;;; test-tui.el --- Floating context regression tests -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Willy Rempel
;; Assisted-by: OpenAI Codex
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'org-xob)
(require 'org-xob-tui)

(defmacro org-xob-tui-test--kb (&rest body)
  "Run BODY with an isolated, in-memory XOB index."
  (declare (indent 0))
  `(let ((org-xob--id-title (make-hash-table :test 'equal))
         (org-id-locations (make-hash-table :test 'equal))
         (org-xob-buffers nil)
         (org-xob-context-display-function nil)
         (org-xob-tui-mode nil))
     (dolist (pair '(("a" . "Alpha") ("b" . "Same title") ("c" . "Same title")))
       (puthash (car pair) (cdr pair) org-xob--id-title))
     ,@body))

(ert-deftest org-xob-context-clean-preview-and-links ()
  (org-xob-tui-test--kb
    (with-temp-buffer
      (org-mode)
      (insert "* Alpha\n:PROPERTIES:\n:ID: a\n:SECRET: hidden\n:END:\n"
              ":BACKLINKS:\n[2026-10-31 Sat] <- [[id:b][Old title]]\n:END:\n"
              ":LOGBOOK:\n[[id:hidden][Private log]]\n:END:\n"
              "A *bold* word and [[id:c][link]] here.\n[[id:c][Repeated]]\n"
              "** Section\n- item\n\n#+begin_src text\n  * literal code\n#+end_src\n")
      (let* ((snapshot (org-xob-context-snapshot "a" (current-buffer)))
             (text (substring-no-properties (plist-get snapshot :text))))
        (should (plist-get snapshot :available))
        (should (string-match-p "A bold word and link here" text))
        (should (string-match-p "Section" text))
        (should (string-match-p "literal code" text))
        (should-not (string-match-p "SECRET\\|PROPERTIES\\|BACKLINKS\\|LOGBOOK\\|Private log\\|\\*\\* Section" text))
        (should (equal (plist-get snapshot :backlinks) '((:id "b" :label "Same title"))))
        (should (equal (plist-get snapshot :forlinks) '((:id "c" :label "Same title"))))))))

(ert-deftest org-xob-context-duplicate-titles-keep-identities ()
  (org-xob-tui-test--kb
    (with-temp-buffer
      (org-mode)
      (insert "* Alpha\n:PROPERTIES:\n:ID: a\n:END:\n[[id:b]] [[id:c]] [[id:missing][Gone]]\n")
      (let ((items (plist-get (org-xob-context-snapshot "a" (current-buffer)) :forlinks)))
        (should (equal (mapcar (lambda (x) (plist-get x :id)) items) '("b" "c" "missing")))
        (should (equal (plist-get (nth 2 items) :label) "Gone"))))))

(ert-deftest org-xob-context-preserves-source-state ()
  (org-xob-tui-test--kb
    (with-temp-buffer
      (org-mode)
      (insert "Preamble\n* Alpha\n:PROPERTIES:\n:ID: a\n:END:\nBody\n** Child\nText\n")
      (search-backward "Text")
      (set-mark (point-max))
      (narrow-to-region (point) (point-max))
      (let ((point (point)) (mark (mark)) (minimum (point-min)) (maximum (point-max))
            (tick (buffer-chars-modified-tick)) (kill-ring '("keep"))
            (windows (window-state-get)))
        (should (equal (org-xob-context-node-at-point) "a"))
        (should (plist-get (org-xob-context-snapshot "a" (current-buffer)) :available))
        (should (= point (point)))
        (should (= mark (mark)))
        (should (= minimum (point-min)))
        (should (= maximum (point-max)))
        (should (= tick (buffer-chars-modified-tick)))
        (should (equal kill-ring '("keep")))
        (should (equal windows (window-state-get)))))))

(ert-deftest org-xob-context-prefers-unsaved-edit ()
  (org-xob-tui-test--kb
    (let ((original (generate-new-buffer " *xob original*"))
          (edit (generate-new-buffer " *xob edit*")))
      (unwind-protect
          (progn
            (with-current-buffer original
              (org-mode) (insert "* Alpha\n:PROPERTIES:\n:ID: a\n:END:\nOld body\n"))
            (with-current-buffer edit
              (org-mode) (insert "* Renamed\n:PROPERTIES:\n:ID: copy\n:EDIT: a\n:END:\nUnsaved body\n"))
            (setq org-xob-buffers (list original edit))
            (let ((snapshot (org-xob-context-snapshot "a" original)))
              (should (equal (plist-get snapshot :title) "Renamed"))
              (should (equal (plist-get snapshot :text) "Unsaved body"))))
        (kill-buffer original) (kill-buffer edit)))))

(ert-deftest org-xob-context-no-export-or-rescan ()
  (org-xob-tui-test--kb
    (with-temp-buffer
      (org-mode)
      (insert "* Alpha\n:PROPERTIES:\n:ID: a\n:END:\n#+INCLUDE: /private/file\n"
              "#+CALL: dangerous()\n#+begin_src emacs-lisp\n(error \"must not run\")\n#+end_src\n")
      (cl-letf (((symbol-function 'org-export-as) (lambda (&rest _) (ert-fail "Export called")))
                ((symbol-function 'org-babel-execute-src-block) (lambda (&rest _) (ert-fail "Babel called")))
                ((symbol-function 'org-id-update-id-locations) (lambda (&rest _) (ert-fail "ID rescan called"))))
        (should (plist-get (org-xob-context-snapshot "a" (current-buffer)) :available))
        (should-not (plist-get (org-xob-context-snapshot "missing") :available))))))

(ert-deftest org-xob-tui-style-needs-no-optional-faces ()
  (with-temp-buffer
    (emacs-tui-view-mode)
    (org-xob-tui--style)
    (should (local-variable-p 'face-remapping-alist))
    (should (assq 'emacs-tui-selection face-remapping-alist))
    (should word-wrap)))

(ert-deftest org-xob-tui-unsupported-display-preserves-layout ()
  (org-xob-tui-test--kb
    (let ((windows (window-state-get)))
      (cl-letf (((symbol-function 'emacs-tui-popup-supported-p) (lambda (&rest _) nil)))
        (should-error (org-xob-tui-mode 1) :type 'user-error))
      (should-not org-xob-tui-mode)
      (should-not org-xob-context-display-function)
      (should (equal windows (window-state-get))))))

(ert-deftest org-xob-tui-routing-avoids-context-writes-and-splits ()
  (org-xob-tui-test--kb
    (let* ((org-xob-auto-dual-pane t) calls
          (org-xob-context-display-function
           (lambda (&rest args) (push args calls))))
      (cl-letf (((symbol-function 'org-xob--show-source-in-buffer)
                 (lambda (&rest _) (ert-fail "Legacy context was written")))
                ((symbol-function 'split-window-right)
                 (lambda (&rest _) (ert-fail "Window split attempted"))))
        (org-xob-show-source 'backlinks t)
        (org-xob-refresh-contexts)
        (org-xob--auto-setup-dual-pane)
        (should-error (org-xob--dual-pane (selected-window)) :type 'user-error)
        (should-error (org-xob--single-pane (selected-window)) :type 'user-error)
        (should (equal (nreverse calls) '((backlinks t) (refresh t))))))))

(ert-deftest org-xob-tui-hide-all-cleans-every-panel-on-error ()
  (with-temp-buffer
    (setq org-xob-tui--sessions '((backlinks . one) (preview . two)))
    (let (closed)
      (cl-letf (((symbol-function 'emacs-tui-live-p) (lambda (_) nil))
                ((symbol-function 'emacs-tui-close)
                 (lambda (session) (push session closed)
                   (when (eq session 'one) (error "Close failed")))))
        (should-error (org-xob-tui-hide-all))
        (should (equal (nreverse closed) '(one two)))
        (should-not org-xob-tui--sessions)))))

(ert-deftest org-xob-tui-pin-and-hidden-preview ()
  (org-xob-tui-test--kb
    (with-temp-buffer
      (setq org-xob-tui--snapshot '(:id "a") org-xob-tui--preview "b")
      (org-xob-tui-pin-preview)
      (org-xob-tui--selected 'forlinks '(:id "c" :label "Same title"))
      (should (equal org-xob-tui--pinned "b"))
      (should (equal org-xob-tui--preview "c"))
      (should-not org-xob-tui--sessions)
      (org-xob-tui-pin-preview)
      (should-not org-xob-tui--pinned))))

(ert-deftest org-xob-tui-disable-cancels-pending-refresh ()
  (with-temp-buffer
    (org-mode)
    (org-xob-tui-context-mode 1)
    (setq org-xob-tui--timer (run-with-idle-timer 60 nil #'ignore))
    (let ((timer org-xob-tui--timer))
      (org-xob-tui-context-mode -1)
      (should-not org-xob-tui--timer)
      (should-not (memq timer timer-idle-list))
      (should-not (memq #'org-xob-tui--after-command post-command-hook)))))

(ert-deftest org-xob-tui-global-disable-tolerates-disposed-view-buffers ()
  (org-xob-tui-test--kb
    (with-temp-buffer
      (let ((view (generate-new-buffer " *xob disposable view*")))
        (unwind-protect
            (progn
              (setq org-xob-tui-mode t)
              (org-xob-tui-context-mode 1)
              (setq org-xob-tui--sessions '((preview . session)))
              ;; Ensure the source is visited before the view it disposes.
              (bury-buffer view)
              (cl-letf (((symbol-function 'emacs-tui-live-p) (lambda (_) nil))
                        ((symbol-function 'emacs-tui-close) (lambda (_) (kill-buffer view))))
                (org-xob-tui-mode -1))
              (should-not (buffer-live-p view))
              (should-not org-xob-tui-mode)
              (should-not org-xob-tui-context-mode)
              (should-not org-xob-tui--sessions))
          (when (buffer-live-p view) (kill-buffer view)))))))

(provide 'test-tui)
;;; test-tui.el ends here
