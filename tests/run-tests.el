;;; run-tests.el --- Test runner for org-xob -*- lexical-binding: t; -*-

;; Configure package management
(require 'package)

;; Add package repositories
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Refresh package contents to get latest package list
(package-refresh-contents)

;; Install required packages
(dolist (pkg '(org-ql hydra pulse))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Install org-super-links from MELPA
(unless (package-installed-p 'org-super-links)
  (package-install 'org-super-links))

;; Load test files
(load-file "tests/test-auto-display.el")
(load-file "tests/test-auto-dual-pane.el")
(load-file "tests/test-bulk-operations.el")
(load-file "tests/test-link-hook.el")
(load-file "tests/test-region-conversion.el")

;; Run tests
(ert-run-tests-batch-and-exit t)
