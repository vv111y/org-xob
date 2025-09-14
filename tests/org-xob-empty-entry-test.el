;;; org-xob-empty-entry-test.el --- tests for org-xob-empty-entry-p -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

;; Load the library while stubbing out heavy dependencies.
(let ((orig-require (symbol-function 'require)))
  (cl-letf (((symbol-function 'require)
             (lambda (feature &optional filename noerror)
               (if (memq feature '(org org-id))
                   (funcall orig-require feature filename noerror)
                 t))))
    (load-file (expand-file-name "../org-xob.el" (file-name-directory load-file-name)))))

(ert-deftest org-xob-empty-entry-p-empty ()
  "Detect empty headings even when blank lines are present."
  (with-temp-buffer
    (org-mode)
    (insert "* Foo\n\n* Bar")
    (goto-char (point-min))
    (should (org-xob-empty-entry-p))))

(ert-deftest org-xob-empty-entry-p-non-empty ()
  "Return nil when heading contains text."
  (with-temp-buffer
    (org-mode)
    (insert "* Foo\nContent\n* Bar")
    (goto-char (point-min))
    (should-not (org-xob-empty-entry-p))))

(provide 'org-xob-empty-entry-test)

;;; org-xob-empty-entry-test.el ends here
