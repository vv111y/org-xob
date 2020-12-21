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

;;; from xob.el

;; uneeded in xob I can just delete stuff
(defun org-xob-hide-backlinks ()
  (interactive))

(defun org-xob-hide-forlinks ()
  (interactive))
;; --------

;; conversion stuff, not needed 

;;;###autoload
(defun org-xob-to-full-node ()
  "Converts node item at point to full node."
  (interactive)
  (org-xob--convert-node-item 'full))

;;;###autoload
(defun org-xob-to-heading-node ()
  "Converts node item at point to a heading."
  (interactive)
  (org-xob--convert-node-item 'heading))

;; TODO check what form at point, call right fn 
(defun org-xob--convert-node-item (to)
  "Converts node item at point to the form specified by 'to'."
  (interactive)
  (let ((item-form ((org-xob--item-form-at-point))))
    (if (org-xob--is-node (point))
        (if (eq (item-form to))
            (message "Node item is already in that form.")
          ())
      )))

;; ---
;; THIS IS FOR THE IMPLEMENTATION THAT ALLOWS FOR EDITABLE NODES
;; replace activate, now that indirect buffer being used
(defun org-xob--node-full (ID)
  "Inserts the full node as a subheading."
  (save-window-excursion
    (save-restriction 
      (org-id-goto ID)
      (org-copy-subtree)
      (org-paste-subtree nil nil nil 'REMOVE)
      (org-entry-put (point) "PARENT" 
                     (org-entry-get (point) "ID" nil nil))
      (org-xob--node-add-timed-property "MODIFIED")
      (org-id-get-create 'FORCE))))

;; TODO check what form at point
;; call right fn 
(defun org-xob--convert-node-item (to)
  "Converts node item at point to the form specified by 'to'."
  (interactive)
  (let ((item-form ((org-xob--item-form-at-point))))
    (if (eq (item-form to))
        (message "Node item is already in that form.")
      ;; a case would be here for the different types
      )))

;;;###autoload
(defun org-xob-clone-node ()
  "Alternative execution for xob. Create a clone for editing.
This does not use the clone transclusion package."
  (interactive)
  (when (not org-xob-on-p)
    (org-xob-start))
  ;; is exobrain started? 
  ;; if point not on node, call get-node
  ;; copy whole node
  ;; generate new ID
  ;; add vparent property, and/or exo-link to parent
  ;; parent backlink has subheading 'clones' 
  )

;; -----------------------
;; not this version
(defun org-xob-context--inline ()
  "Show the contextual nodes as a subheading."
  )

(defun org-xob-context--outline ()
  "Show the contextual nodes as adjacent headings."
  )
;; -----------------------

(defun org-xob-refresh-sources ()
  "Show the contextual nodes as adjacent headings."
  )

(defun org-xob-to-full-node ()
  "Converts node item at point to full node."
  (interactive)
  (let ((ID (org-id-get nil nil nil)))
    () ;; delete item 
    (org-xob--node-full (org-id-get nil nil nil))))

;;;;; Edit syncing not for v0.5


(defun org-xob--sync-edits (beg end len)
  (goto-char beg)
  ;; (if org-xob--kb-node-p)
  (if (member "kb" (org-get-tags))
      (org-xob--sync-node)))

(defun org-xob--sync-node ())

;; TODO replace? yes not using now
(defun org-xob--activate-node (ID)
  "Copies KB node with ID to current location and sets
appropriate properties as a derivative node."
  ;; TODO make main buffer, and whatever else for sync-editing style
  (org-xob--node-full))

(cl-defstruct node title type backlinks)

;;;;; context copies using org-copy-subtree
;; -----------------------------------------------------------------
;; FAIL it copies the heading and prop drawer as well. dupe
;; TODO prob just make custom region and copy
;; this way could be used to get meta data
;;;###autoload
(defun org-xob-to-section ()
  "Get the whole top section before any headings underneath."
  (interactive)
  (save-excursion
    (save-restriction
      (org-id-goto (org-entry-get (point) "PID"))
      (org-copy-subtree nil nil nil 'nosubtrees)))
  (org-xob-to-heading)
  (org-paste-subtree nil nil nil 'REMOVE)
  (org-entry-put (point) "PID"
                 (org-entry-get (point) "ID" nil nil))
  (org-entry-delete "ID"))

;;;###autoload
(defun org-xob-to-full-node (ID)
  "Converts node item at point to full node. The ID is still modified as
it is still a copy, however all other property drawer contents is unchanged."
  (interactive)
  (save-excursion
    (save-restriction 
      (org-id-goto (org-entry-get (point) "PID"))
      (org-copy-subtree)))
  (org-paste-subtree nil nil nil 'REMOVE)
  (org-entry-put (point) "PID" 
                 (org-entry-get (point) "ID" nil nil))
  (org-entry-delete "ID"))
;; -----------------------------------------------------------------
