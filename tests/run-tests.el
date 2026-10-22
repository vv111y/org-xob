;;; tests/run-tests.el --- CI test runner for org-xob -*- lexical-binding: t; -*-

;; Keep package files inside the repo so GitHub Actions can cache them.
(defvar run-tests--script-dir (file-name-directory (or load-file-name buffer-file-name)))
(defvar run-tests--project-root (expand-file-name ".." run-tests--script-dir))
(defvar run-tests--elpa-dir (expand-file-name "elpa" run-tests--project-root))
(defvar run-tests--straight-dir (expand-file-name "straight" run-tests--project-root))

(setq package-user-dir run-tests--elpa-dir)
(setq package-enable-at-startup nil)

(require 'package)

;; Configure package archives (kept for backwards compatibility and optional packages)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Initialize package system (harmless if already initialized)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; --- Bootstrap straight.el into a project-local directory ---
;; Ensure the directory path ends with a separator; straight.el expects
;; user-emacs-directory to be a directory path (with trailing slash).
(let* ((run-tests--straight-dir (expand-file-name "straight" run-tests--project-root))
       (user-emacs-directory (file-name-as-directory run-tests--straight-dir))
       (bootstrap-file (expand-file-name "repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (message "Bootstrapping straight.el into %s" user-emacs-directory)
    ;; Avoid symlink creation on CI; copy files instead (more portable on runners).
    (setq straight-use-symlinks nil)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  ;; If bootstrap still failed to produce the expected bootstrap.el, fall back
  ;; to directly cloning the straight.el repo into straight/repos/straight.el.
  (unless (file-exists-p bootstrap-file)
    (message "straight bootstrap file missing; attempting git clone fallback")
    (let ((target (expand-file-name "repos/straight.el" user-emacs-directory)))
      (unless (file-directory-p target)
        (message "Cloning straight.el -> %s" target)
        (unless (zerop (call-process "git" nil nil nil "clone" "--depth" "1"
                                    "https://github.com/raxod502/straight.el" target))
          (message "Warning: git clone failed for straight.el")))
      (let ((bf (expand-file-name "bootstrap.el" target)))
        (when (file-exists-p bf)
          (setq bootstrap-file bf)))))
  ;; Load the bootstrap file if present, otherwise signal a clear error.
  (if (file-exists-p bootstrap-file)
      (load bootstrap-file nil 'nomessage)
    (error "straight.el bootstrap failed and fallback didn't produce %s" bootstrap-file)))

;; Integrate use-package with straight (optional but convenient)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Packages required by the project and tests. Add others as CI reports them missing.
(defvar run-tests--required-packages
  '(compat dash helm hydra org-ql org-ql-search org-super-links s transient
           pulse use-package org-id))

;; Recipes for packages that live on GitHub (not on MELPA/GNU).
;; Each value is a straight recipe list (with the package symbol as the car).
(defvar run-tests--straight-recipes
  '((org-super-links . (org-super-links :type git :host github :repo "toshism/org-super-links"))
    ;; org-ql-search is provided by the org-ql project; map it to that repo if needed.
    (org-ql-search   . (org-ql-search :type git :host github :repo "alphapapa/org-ql"))))

;; Ensure required packages via straight (preferred) and fall back to package.el if needed.
(dolist (p run-tests--required-packages)
  (let ((recipe (cdr (assoc p run-tests--straight-recipes))))
    (condition-case err
        (progn
          (if recipe
              (straight-use-package (car recipe) (cdr recipe))
            (straight-use-package p)))
      (error
       (message "Warning: straight failed to install %s: %S. Trying package.el as fallback." p err)
       (condition-case err2
           (unless (package-installed-p p)
             (package-refresh-contents)
             (package-install p))
         (error (message "Warning: package.el also failed to install %s: %S" p err2)))))))

;; If org-super-links or other optional packages are still missing, provide minimal
;; stubs so the package can load and headless tests run. This avoids immediate load
;; failures and lets ERT surface functional test failures instead.
(unless (require 'org-super-links nil t)
  (message "org-super-links not available; installing fallback stubs")
  (defun org-super-links-link (&rest _) nil)
  (defun org-super-links-store-link (&rest _) nil)
  (defun org-super-links-insert-link (&rest _) nil)
  (setq org-super-links-backlink-into-drawer nil
        org-super-links-link-prefix nil
        org-super-links-link-postfix nil
        org-super-links-backlink-postfix nil
        org-super-links-related-into-drawer nil)
  (provide 'org-super-links))

(unless (require 'org-ql-search nil t)
  (when (require 'org-ql nil t)
    (message "org-ql-search not available; aliasing to org-ql-select")
    (defalias 'org-ql-search 'org-ql-select)
    (provide 'org-ql-search)))

(unless (require 'org-id nil t)
  (message "org-id not available; providing minimal stubs")
  (defun org-id-get (&rest _) nil)
  (defun org-id-store (&rest _) nil)
  (provide 'org-id))

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
