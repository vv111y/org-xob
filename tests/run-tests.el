;;; run-tests.el --- Batch test runner for org-xob -*- lexical-binding: t; -*-

;;; Commentary:
;; GitHub Actions entry point.  Installs runtime test dependencies, loads
;; org-xob and every test-*.el file, then runs any ERT tests that were defined.

;;; Code:

(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

(dolist (package '(dash helm hydra org-ql org-super-links s transient))
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(require 'org-xob)
(require 'ert)

(dolist (test-file (directory-files (expand-file-name "." (file-name-directory load-file-name))
                                    t
                                    "\\`test-.*\\.el\\'"))
  (message "Loading test file: %s" test-file)
  (load test-file nil nil t))

(ert-run-tests-batch-and-exit t)

;;; run-tests.el ends here
