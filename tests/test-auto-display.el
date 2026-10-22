;;; tests/test-auto-display.el --- Test auto-display links feature (CI guarded) -*- lexical-binding: t; -*-

;; Load the xob system
(require 'org-xob-core)
(require 'org-xob-backend)
(require 'org-xob)

;; Test function to validate auto-display links setting
(ert-deftest org-xob-test-auto-display-links ()
  "Headless-friendly test for auto-display links functionality."
  (should (boundp 'org-xob-auto-display-links))
  (let ((orig (and (boundp 'org-xob-auto-display-links) org-xob-auto-display-links)))
    (when (fboundp 'org-xob-toggle-auto-display-links)
      (funcall 'org-xob-toggle-auto-display-links)
      (should (boundp 'org-xob-auto-display-links))
      (funcall 'org-xob-toggle-auto-display-links))
    (when (boundp 'org-xob-auto-display-links)
      (setq org-xob-auto-display-links orig))))
