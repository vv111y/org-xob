;; Redesigned file management for org-xob
;; This is a proposal for how the file management should work

(defun org-xob--scan-and-register-files ()
  "Scan directory and register all xob files. Create missing files if needed.
This replaces both org-xob--register-files and org-xob--process-files."
  (org-xob--clear-file-variables)
  
  ;; First pass: scan existing files and register them
  (dolist (filepath (directory-files org-xob-dir t "\\.org$"))
    (unless (string-match-p "#" filepath)
      (org-xob--register-single-file filepath)))
  
  ;; Second pass: ensure we have current files of each type
  (org-xob--ensure-current-files))

(defun org-xob--register-single-file (filepath)
  "Register a single file based on its properties."
  (with-temp-buffer
    (insert-file-contents-literally filepath nil 0 1024)
    (let* ((props (car (org-collect-keywords '("PROPERTY"))))
           (is-xob (member "xob t" props))
           (is-current (member "xob-current-file t" props)))
      (when is-xob
        (cond
         ((member "xob-log t" props)
          (push filepath org-xob--log-files)
          (when is-current (setq org-xob--log-file filepath)))
         ((member "xob-agenda t" props)
          (push filepath org-xob--agenda-files)
          (when is-current (setq org-xob--agenda-file filepath)))
         ((member "xob-archive t" props)
          (push filepath org-xob--archive-files)
          (when is-current (setq org-xob--archive-file filepath)))
         (t
          (push filepath org-xob--KB-files)
          (when is-current (setq org-xob--KB-file filepath))))))))

(defun org-xob--ensure-current-files ()
  "Ensure we have a current file for each type. Create if missing."
  (let ((file-specs '((org-xob--agenda-file  org-xob--agenda-filename-prefix  org-xob--agenda-files  org-xob--agenda-header)
                      (org-xob--log-file     org-xob--log-filename-prefix     org-xob--log-files     org-xob--log-header)
                      (org-xob--archive-file org-xob--archive-filename-prefix org-xob--archive-files org-xob--archive-header)
                      (org-xob--KB-file      org-xob--KB-filename-prefix      org-xob--KB-files      nil))))
    (dolist (spec file-specs)
      (let ((current-var (nth 0 spec))
            (prefix-var (nth 1 spec))
            (list-var (nth 2 spec))
            (type-header (nth 3 spec)))
        (unless (and (symbol-value current-var)
                     (file-exists-p (symbol-value current-var)))
          (org-xob--create-new-file current-var prefix-var list-var type-header))))))

(defun org-xob--create-new-file (current-var prefix-var list-var type-header)
  "Create a new file of the specified type."
  (let* ((prefix (symbol-value prefix-var))
         (existing-files (symbol-value list-var))
         (file-number (+ 1 (length existing-files)))
         (filename (format "%s%03d.org" prefix file-number))
         (filepath (expand-file-name filename org-xob-dir)))
    
    ;; Remove current status from any existing file of this type
    (when (symbol-value current-var)
      (org-xob--remove-current-status (symbol-value current-var)))
    
    ;; Create the new file
    (with-temp-file filepath
      (insert org-xob--xob-header)
      (when type-header
        (insert type-header))
      (insert org-xob--current-header))
    
    ;; Update the variables
    (add-to-list list-var filepath)
    (set current-var filepath)
    
    (message "XOB: Created new %s file: %s" 
             (symbol-name current-var) filename)
    filepath))

(defun org-xob--remove-current-status (filepath)
  "Remove xob-current-file property from a file."
  (when (and filepath (file-exists-p filepath))
    (with-current-buffer (find-file-noselect filepath)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+PROPERTY: xob-current-file t$" nil t)
          (delete-region (line-beginning-position) (1+ (line-end-position)))
          (save-buffer))))))

;; Key improvements:
;; 1. Single function handles both scanning and ensuring files exist
;; 2. Consistent use of full file paths everywhere
;; 3. Clear separation of concerns
;; 4. No complex validation logic - just check if file exists
;; 5. Proper handling of current file status
;; 6. Better error handling and messaging
