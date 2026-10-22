;;; test-empty-entry.el --- Empty heading navigation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-xob-core)

(ert-deftest org-xob-empty-entry-ignores-blank-lines ()
  (with-temp-buffer
    (org-mode)
    (insert "* Empty\n \t\n\n* Sibling\nBody\n")
    (goto-char (point-min))
    (should (org-xob-empty-entry-p))))

(ert-deftest org-xob-empty-entry-at-end-of-buffer ()
  (dolist (text '("* Empty" "* Empty\n" "* Empty\n \t\n\n"))
    (with-temp-buffer
      (org-mode)
      (insert text)
      (goto-char (point-min))
      (should (org-xob-empty-entry-p)))))

(ert-deftest org-xob-empty-entry-with-heading-trailing-whitespace ()
  (dolist (text '("* Empty  \t" "* Empty  \t\n\n* Sibling\n"))
    (with-temp-buffer
      (org-mode)
      (insert text)
      (goto-char (point-min))
      (should (org-xob-empty-entry-p)))))

(ert-deftest org-xob-empty-entry-rejects-body-text ()
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n\nBody\n \t\n* Sibling\n")
    (goto-char (point-min))
    (should-not (org-xob-empty-entry-p))))

(ert-deftest org-xob-empty-entry-rejects-child-headings ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n\n** Child\n\n* Sibling\n")
    (goto-char (point-min))
    (should-not (org-xob-empty-entry-p))))

(ert-deftest org-xob-empty-entry-rejects-metadata ()
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\n:PROPERTIES:\n:ID: example\n:END:\n\n")
    (goto-char (point-min))
    (should-not (org-xob-empty-entry-p))))

(ert-deftest org-xob-empty-entry-requires-heading ()
  (with-temp-buffer
    (org-mode)
    (insert "* Heading\nBody\n")
    (goto-char (point-min))
    (forward-line 1)
    (should-not (org-xob-empty-entry-p))))

(ert-deftest org-xob-empty-entry-preserves-buffer-and-point ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Empty\n \t\n** Sibling\n")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 4)
    (let ((position (point))
          (text (buffer-string)))
      (should (org-xob-empty-entry-p))
      (should (= (point) position))
      (should (equal (buffer-string) text)))))

(ert-deftest org-xob-up-heading-skips-empty-entry ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\n** Empty\n \t\n** Sibling\n")
    (goto-char (point-min))
    (forward-line 1)
    (org-xob--up-heading)
    (should (= (point) (point-min)))))

(ert-deftest org-xob-up-heading-folds-children-before-moving-up ()
  (with-temp-buffer
    (org-mode)
    (insert "* Root\n** Parent\n\n*** Child\nBody\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((position (point)))
      (org-xob--up-heading)
      (should (= (point) position))
      (should (org-xob-folded-p))
      (org-xob--up-heading)
      (should (= (point) (point-min))))))

(provide 'test-empty-entry)
;;; test-empty-entry.el ends here
