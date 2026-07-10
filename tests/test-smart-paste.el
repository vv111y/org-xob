;;; test-smart-paste.el --- Tests for smart top-section paste -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for pasting mixed Org clips into an XOB node top section.

;;; Code:

(require 'ert)
(require 'org)
(require 'cl-lib)
(require 'org-xob-backend)

(defmacro org-xob-test--with-silent-modified-time (&rest body)
  "Run BODY without requiring an XOB node for modified-time updates."
  `(cl-letf (((symbol-function 'org-xob--update-modified-time) #'ignore))
     ,@body))

(defun org-xob-test--paste-top-section (initial clip)
  "Paste CLIP at the top section of INITIAL's first heading.
Return the resulting buffer text."
  (with-temp-buffer
    (org-mode)
    (insert initial)
    (goto-char (point-min))
    (org-xob-test--with-silent-modified-time
     (org-xob--paste-top-section clip))
    (buffer-string)))

(ert-deftest org-xob-smart-paste-mixed-headings-destination-with-no-children ()
  "A mixed clip's minimum heading becomes a child of a childless destination."
  (should
   (equal
    (org-xob-test--paste-top-section
     "* Destination\nTop section.\n"
     "Intro text.\n* Imported\nBody.\n")
    "* Destination\nTop section.\nIntro text.\n** Imported\nBody.\n")))

(ert-deftest org-xob-smart-paste-mixed-headings-destination-with-existing-children ()
  "A mixed clip is inserted before existing children in the destination top section."
  (should
   (equal
    (org-xob-test--paste-top-section
     "* Destination\nTop section.\n** Existing child\nChild body.\n"
     "Intro text.\n* Imported\nBody.\n")
    "* Destination\nTop section.\n\nIntro text.\n** Imported\nBody.\n\n** Existing child\nChild body.\n")))

(ert-deftest org-xob-smart-paste-mixed-headings-preserves-leading-text ()
  "Leading non-heading text remains top-section content before normalized headings."
  (should
   (equal
    (org-xob-test--paste-top-section
     "* Destination\n"
     "Leading paragraph.\n- list item\n** Imported\nBody.\n")
    "* Destination\nLeading paragraph.\n- list item\n** Imported\nBody.\n")))

(ert-deftest org-xob-smart-paste-mixed-headings-preserves-relative-nesting ()
  "Nested headings in a mixed clip retain their relative depth."
  (should
   (equal
    (org-xob-test--paste-top-section
     "* Destination\n"
     "Intro text.\n** Imported\n*** Nested\nNested body.\n")
    "* Destination\nIntro text.\n** Imported\n*** Nested\nNested body.\n")))

(ert-deftest org-xob-smart-paste-no-heading-clip-inserts-plain-text ()
  "A clip with no headings continues to insert without normalization."
  (should
   (equal
    (org-xob-test--paste-top-section
     "* Destination\n"
     "Plain text only.\n")
    "* Destination\nPlain text only.\n\n")))(provide 'test-smart-paste)
;;; test-smart-paste.el ends here
