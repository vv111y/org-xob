;; Helper function to clean up duplicate properties in org-xob files
(defun org-xob-clean-duplicate-properties ()
  "Remove duplicate xob properties from all org files in org-xob-dir."
  (interactive)
  (when (and org-xob-dir (file-directory-p org-xob-dir))
    (dolist (file (directory-files org-xob-dir t "\\.org$"))
      (unless (string-match-p "#" file)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (let ((found-properties '())
                  (changes-made nil))
              ;; Find all property lines and track what we've seen
              (while (re-search-forward "^#\\+PROPERTY: \\(xob[^ ]*\\) t$" nil t)
                (let ((prop (match-string 1))
                      (line-start (line-beginning-position)))
                  (if (member prop found-properties)
                      ;; Duplicate found, remove this line
                      (progn
                        (delete-region line-start (1+ (line-end-position)))
                        (setq changes-made t)
                        (message "Removed duplicate property %s from %s" prop (file-name-nondirectory file))
                        ;; Move back one line since we deleted the current line
                        (when (> (point) (point-min))
                          (forward-line -1)))
                    ;; First occurrence, add to found list and continue
                    (push prop found-properties))))
              (when changes-made
                (save-buffer)
                (message "Cleaned duplicate properties in %s" (file-name-nondirectory file))))))))))

;; Function to check what org-xob-dir is currently set to
(defun org-xob-check-directory ()
  "Check the current org-xob directory and list files found."
  (interactive)
  (message "org-xob-dir is set to: %s" org-xob-dir)
  (if (file-directory-p org-xob-dir)
      (let ((org-files (directory-files org-xob-dir t "\\.org$")))
        (message "Found %d .org files in directory:" (length org-files))
        (dolist (file org-files)
          (message "  - %s" file)))
    (message "Directory does not exist: %s" org-xob-dir)))

;; Function to show the first few lines of each org file to check properties
(defun org-xob-show-file-properties ()
  "Show the properties section of each org file in org-xob-dir."
  (interactive)
  (when (and org-xob-dir (file-directory-p org-xob-dir))
    (dolist (file (directory-files org-xob-dir t "\\.org$"))
      (unless (string-match-p "#" file)
        (with-temp-buffer
          (insert-file-contents file nil 0 500) ; Read first 500 chars
          (message "\n--- %s ---" (file-name-nondirectory file))
          (message "%s" (buffer-string)))))))
