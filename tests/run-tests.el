;;; tests/run-tests.el --- CI test runner for org-xob -*- lexical-binding: t; -*-

;; Keep package files inside the repo so GitHub Actions can cache them.
(defvar run-tests--script-dir (file-name-directory (or load-file-name buffer-file-name)))
(defvar run-tests--project-root (expand-file-name ".." run-tests--script-dir))
(defvar run-tests--elpa-dir (expand-file-name "elpa" run-tests--project-root))

(setq package-user-dir run-tests--elpa-dir)
(setq package-enable-at-startup nil)

(require 'package)

;; Repositories
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Initialize package system (harmless if already initialized)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Refresh archive contents only when necessary
(defun run-tests--ensure-archive-contents ()
  (unless package-archive-contents
    (message "Refreshing package archives...")
    (condition-case err
        (package-refresh-contents)
      (error
       (message "package-refresh-contents failed: %S" err)))))

;; Ensure a package is installed; refresh archives if needed.
;; We do NOT abort the whole run if a package is unavailable — some packages
;; may be optional for headless CI. Instead we warn and continue so tests can
;; run and ERT will report failures.
(defun run-tests--ensure-package (pkg)
  (unless (package-installed-p pkg)
    (run-tests--ensure-archive-contents)
    (condition-case err
        (package-install pkg)
      (error
       (message "Warning: Failed to install package %S: %S. Continuing; tests may fail if this package is required." pkg err)))))

;; Packages required by the project and tests. Add others as CI reports them missing.
(defvar run-tests--required-packages
  '(compat dash helm hydra org-ql org-ql-search org-super-links s transient
           pulse use-package org-id))

(dolist (p run-tests--required-packages)
  (run-tests--ensure-package p))

;; Add project and tests dir to load-path
(add-to-list 'load-path (expand-file-name run-tests--project-root))
(add-to-list 'load-path (expand-file-name "tests" run-tests--project-root))

;; Load the project (adjust if the main library has a different name)
(condition-case err
    (require 'org-xob nil t)
  (error
   (message "Warning: could not (require 'org-xob): %S" err)))

;; Load all test-*.el files in tests/
(let ((tests-dir (expand-file-name "tests" run-tests--project-root)))
  (when (file-directory-p tests-dir)
    (dolist (f (sort (directory-files tests-dir t "^test-.*\\.el$") 'string<))
      (message "Loading test file: %s" f)
      (load f nil nil t))))

;; Run ERT and exit with a non-zero code on failures
(require 'ert)
(ert-run-tests-batch-and-exit t)
