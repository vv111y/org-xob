;;; axiv
;;;; yes/no - use org-capture instead

(if (not sel in all-nodes)
    (if (y-or-n-p-with-timeout "create new node? " 6 nil)
        (org-xob-add-node sel)))
;;;; error trying to start emacs as a separate process
;; emacs: Terminal type "dumb" is not powerful enough to run Emacs.
;; It lacks the ability to position the cursor.
;; If that is not the actual type of terminal you have,
;; use the Bourne shell command 'TERM=...; export TERM' (C-shell:
;; 'setenv TERM ...') to specify the correct type.  It may be necessary
;; to do 'unset TERMINFO' (C-shell: 'unsetenv TERMINFO') as well.

;; Process ttt exited abnormally with code 1
;; (with-emacs-server)



;;;; hashtable load NO

;; NO, easier way
(defun org-xob--table-load ()
  "Load the node lookup hash table."
  (setq org-xob--title-id nil)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents org-id-locations-file)
          (setq org-id-locations (read (current-buffer)))
          (let ((loc (file-name-directory org-id-locations-file)))
            (mapc (lambda (item)
                    (unless (file-name-absolute-p (car item))
                      (setf (car item) (expand-file-name (car item) loc))))
                  org-id-locations)))
      (error
       (message "Could not read org-id-values from %s.  Setting it to nil."
                org-id-locations-file))))
  (setq org-id-files (mapcar 'car org-id-locations))
  (setq org-id-locations (org-id-alist-to-hash org-id-locations)))
;;; pulse highlight indicator
(pulse-momentary-highlight-one-line (point))
(pulse-lighten-highlight)

;;; org-ql 
(require 'org-ql)
(org-ql-search 'all
  '(property "CPARENTS"))
;;; uneeded in xob
;; I can just delete stuff
(defun org-xob-hide-backlinks ()
  (interactive))

(defun org-xob-hide-forlinks ()
  (interactive))
