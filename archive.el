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
;;;;; main viable context copies 
;; TODO for all, should only do at a subheading, at source head do map 
;;;###autoload
(defun org-xob-clear-heading ()
  "Converts subtree to the headline and properties drawer only.
This is idempotent and application to such a heading makes no change.
This can be applied to heading at point or used in a mapping."
  (interactive)
  (org-with-wide-buffer
   (org-back-to-heading t)
   (org-mark-subtree)
   (org-end-of-meta-data 1)
   (call-interactively #'delete-region))
  (goto-char (- (point) 1)))

;; TEST 
;;;###autoload
(defun org-xob-to-summary ()
  "Show backlinks with summaries. This is defined as the first paragraph if it exists."
  (interactive)
  (save-excursion
    ;; if on source main head, map, if on sub head do this.
    (org-xob-clear-heading)
    (org-end-of-meta-data 1)
    (insert
     (org-id-goto (org-entry-get (point) "PID"))
     (org-with-wide-buffer
      (org-end-of-meta-data 'full)
      (let ((p (org-element-at-point)))
        (buffer-substring-no-properties (org-element-property :contents-begin p)
                                        (org-element-property :contents-end p)))))))

;; TEST 
;;;###autoload
(defun org-xob-to-node-tree ()
  "Show only subheadings of node."
  (interactive)
  (org-xob-clear-heading)
  (org-end-of-meta-data 1)
  (newline)
  (insert
   (let ((str))
     (org-id-goto (org-entry-get (point) "PID"))
     (org-with-wide-buffer
      (org-narrow-to-subtree)
      (org-map-tree (lambda ()
                      (setq str (concat str 
                                        (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position)
                                         "\n"))))))
     str)))

;; trying without, just passing lambdas
(defmacro org-xob--copypaste-from-node (&rest body)
  "Apply &body at the source node which should return a string
that will overwrite current source tree item."
  ;; (declare (debug (body)))
  `(let ((func (lambda () (progn 
                            (org-xob-clear-heading)
                            (org-end-of-meta-data)
                            (insert
                             (save-excursion
                               (org-id-goto (org-entry-get (point) "PID"))
                               (org-with-wide-buffer
                                (org-narrow-to-subtree)
                                (org-end-of-meta-data 1)
                                ,@body)
                               ))))))
     (if (org-xob--is-source-p)
         (org-xob--map-source func)
       (funcall func))))


;; org-xob--map-source cut, didn't need it afterall
;; (org-narrow-to-subtree)
;; (outline-show-all)
;; (outline-next-heading)



(defun org-xob--save-state ()
  "Save exobrain state."
  (if (not (file-directory-p org-xob--workspace))
      (make-directory org-xob--workspace))
  (if (not (file-directory-p org-xob-path))
      (make-directory org-xob-path))
  (cl-loop for (k . v) in org-xob--objects
           do (org-xob--save-object (concat org-xob-path v) k)))

;; old way for inserting link
(org-insert-link nil (concat "ID:" ID) (org-xob--ID-title ID))

(defvar org-xob--auto-types '(
                              ("ad" . a.day)
                              ("session" . a.session)  								;; 
                              ("project" . a.project)									;; 
                              ("log" . a.log) 												;; 
                              ("log personal" . a.log.life)						;; rundschau too
                              ("log it tools" . a.log.it-tools) 			;; 
                              ("log tools" . a.log.tools)  						;; 
                              ("log project" . a.log.project)					;; 
                              ("article" . n.bib.article)							;; 
                              ("webpage" . n.bib.web)									;; 
                              ("fast" . n.n)													;; 
                              ("topic" . n.topic)											;; 
                              ))

;;; old node edit  
;; old minor mode
;; (add-hook 'after-change-functions #'org-xob--sync-edits)
;; (add-hook 'after-change-functions (lambda () set (make-local-variable 'org-xob-syncedp nil 'APPEND 'LOCAL)))
;; (setq-local org-xob-syncedp nil)

;; primary fn to capture every single edit
;; (remove-hook 'after-change-functions #'org-xob--sync-edits)

;;; capture stuff 
;; capture hook belongs in finalize
;; (add-hook 'org-capture-mode-hook #'org-xob--new-node)

;; :func (lambda () (progn
;;                     (org-insert-subheading '(4))
;;                     (insert "hello world")))

;;; alternative open logfile and have a pointer 

(setq org-xob--log 
      (find-file-noselect (concat org-xob-path org-xob--log-file)))

;; setq returns value
(setq vvm (setq vv "test"))

(setq vvm '(1 2 3 4))
(member 5 vvm)
;;; remove node option
;; thos option removes both for and backlinks, 
(defun org-xob-remove-node (&optional ID)
  "Removes node at point from xob system, but does not delete the node itself.
Removes node from the hash tables, and any backlinks and forlinks in other nodes
referencing it. If called with optional ID argument, then remove the node with that ID."
  (interactive)
  (unless org-xob-on-p (org-xob-start))
  (save-window-excursion
    (save-excursion
      (if ID (org-id-goto ID))
      (let* ((ID (org-id-get (point)))
             (title (gethash ID org-xob--id-title))
             (backlinks (org-xob--node-get-links "backlinks"))
             (forelinks (org-xob--node-get-links "forelinks"))
             link-element)
        (dolist (linktype '(forelinks backlinks))
          (dolist (el linktype)
            (org-id-goto el)
            (save-restriction
              (org-narrow-to-subtree)
              (outline-show-all)
              (setq link-element (org-super-links--find-link ID))
              (if link-element
                  (cond ((eq linktype 'forelinks)
                         (org-super-links--delete-link link-element))
                        ((eq linktype 'backlinks)
                         (let* ((elem (org-element-context))
                                (beg (org-element-property :contents-begin elem))
                                (end  (org-element-property :contents-end elem))
                                (link (org-element-property :path elem))
                                text
                                (link-begin (org-element-property :begin elem))
                                (link-end (org-element-property :end elem))
                                )
                           (if (and beg end)
                               (setq text (concat (buffer-substring-no-properties beg end)
                                                  " "))
                             (setq text (concat link " ")))
                           (delete-region link-begin link-end)
                           (insert text))))))))
        (remhash ID org-xob--id-title)
        (remhash title org-xob--title-id)
        (org-entry-put (point) "ID" "")
        (org-id-update-id-locations (list (buffer-file-name)) 'silent)
        (org-xob--save-state)))))
;;; xob-start
(defun org-xob-start ()
  "Start the xob system: load state or initialize new. Open new day node."
  (interactive)
  (if (and
       (if org-xob-on-p (progn (message "XOB: already started.") nil) t)
       (and
        (add-hook 'org-capture-prepare-finalize-hook #'org-xob--new-node)
        (add-hook 'org-follow-link-hook #'org-xob--link-hook-fn)
        (message "XOB: hooks enabled."))
       (if (file-directory-p org-xob-dir) (message "XOB: directory found.")
         (prog1 (message "XOB: directory not found, creating.")
           (make-directory org-xob-dir t)))
       (cl-loop for (k . v) in org-xob--objects
                do (if (file-exists-p (concat org-xob-dir v))
                       (prog1 (message "XOB: found %s" v)
                         (org-xob--load-object v k))
                     (progn
                       (message "XOB: file %s missing, initializing new %s" v k)
                       (cond
                        ((equal "org-xob--KB-file" (symbol-name k))
                         (set k (org-xob--new-KB-file)))
                        ((equal "org-xob--KB-files" (symbol-name k))
                         (set k nil))
                        ((or (equal "org-xob--title-id" (symbol-name k))
                             (equal "org-xob--id-title" (symbol-name k)))
                         (set k (make-hash-table
                                 :test 'equal
                                 :size org-xob--table-size))))))
                finally return t)
       (if (file-exists-p (concat org-xob-dir org-xob--log-file))
           (message "XOB: found log file.")
         (with-temp-file (concat org-xob-dir org-xob--log-file)
           (message "XOB: log file missing, initializing new.")
           (insert "") t))
       (setq org-id-extra-files org-xob--KB-files)
       (setq org-xob--kb-file-counter (length org-xob--KB-files))
       (setq org-xob-today-string (concat "[" (format-time-string "%F %a") "]"))
       (and
        (or
         (setq org-xob-today (gethash org-xob-today-string
                                      org-xob--title-id))
         (setq org-xob-today (org-xob--capture "ad")))
        (save-window-excursion
          (setq org-xob-today-buffer
                (find-file (concat org-xob-dir org-xob--log-file))))
        (message "XOB: Todays log entry opened."))
       (and
        (if (file-exists-p (concat org-xob-dir org-xob--agenda-file))
            (message "XOB: found xob agenda file.")
          (with-temp-file (concat org-xob-dir org-xob--agenda-file)
            (message "XOB: xob agenda file missing, initializing new.")
            (insert "") t))
        (unless (member org-xob--agenda-file org-agenda-files)
          (push (concat org-xob-dir org-xob--agenda-file) org-agenda-files))))
      (prog1
        (setq org-xob-on-p t)
        (message "XOB: started."))
    (message "XOB: Unable to (re)start.")))
;;; custom org-id

(defvar org-xob--id-locations (make-hash-table :test 'equal)
  "hashtable to store heading ID and buffer pairs. Speeds up finding
xob meta headings. Point is not kept as headings are allowed to be moved
within their buffers.")

(defun org-xob--id-create (&optional POM)
  "Create a UUID formatted ID. With optional POM, create an ID property at 
POM if it is an org heading. org-id will not work with buffers that are
not visiting a file. This function is meant for such a case. Use in conjunction
with org-xob--id-goto to return to this heading. Returns ID regardless."
  (let ((ID (uuidgen-4))
        (POM (if POM POM (point-marker)))
        (mbuf (if (and POM (markerp POM))
                  (marker-buffer POM)
                (current-buffer))))
    (save-window-excursion
      (save-excursion
        (with-current-buffer mbuf
          (goto-char POM)
          ;; (goto-char (marker-position POM))
          (if (org-at-heading-p)
              (org-entry-put (point) "ID" ID)
            (message "POM is not an org heading. No ID created."))
          )))
    (puthash ID mbuf org-xob--id-locations)
    ID))

(defun org-xob--id-goto (ID)
  "Search buffers for org heading with ID and place point there.
Return true if found, nil otherwise."
  ;; todo remove every source
  (let ((buf (gethash ID org-xob--id-locations))
        (place nil))
    (if (bufferp buf)
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (if (re-search-forward ID nil 'noerror nil)
               (progn 
                 (org-back-to-heading)
                 (setq place (point)))))))
    (if place (progn
                (set-buffer buf)
                (goto-char place)
                t)
      (message "XOB: cannot find buffer associated with heading %s." ID) nil)))

(defun org-xob--id-goto (sID)
  "TODO changed: goto context buffer and then look
Search buffers for org heading with ID and place point there.
Return point position if found, nil otherwise."
  (let (func (lambda (org-with-wide-buffer
                      (goto-char (point-min))
                      (if (re-search-forward (rx (and
                                                  ":ID:"
                                                  (one-or-more space)
                                                  sID))
                                             nil 'noerror nil)
                          (progn 
                            (org-back-to-heading)
                            (point))
                        nil)))))
  (or (and sID
           (equal sID (org-entry-get (point) "ID"))
           (point))
      (funcall func)
      (and (set-buffer (org-xob--other-buffer))
           (funcall func))))
;;; toggle window alt

(select-window org-xob--sideline-window)
(switch-to-buffer org-xob--context-buffer t t)

;;; full node hacking

;; on calling end:
;; (org-yank)
;; (kill-new (funcall payload))
;; (if payload
;;     (let (str
;;           (org-yank-folded-subtrees t)
;;           (org-yank-adjusted-subtrees t))
;;       (save-excursion
;;         (org-id-goto (org-entry-get (point) "PID"))
;;         (org-with-wide-buffer
;;          (org-save-outline-visibility
;;              (org-narrow-to-subtree)
;;            (setq str (funcall payload))
;;            (deactivate-mark 'force))))
;;       (if (stringp str)
;;           (insert str))
;;       ))

(defun org-xob-to-full-node ()
  "Show the full KB node, excepting properties drawer, planning & clocking information."
  (interactive)
  (org-xob--kb-copy-paste
   #'(lambda ()

       (let (
             (org-yank-folded-subtrees nil)
             (org-yank-adjusted-subtrees t)
             str
             ;; (
              ;; str
                       ;; (progn
                       ;;       (org-copy-subtree)
                       ;;       ;; (org-mark-subtree)
                       ;;       ;; (org-end-of-meta-data t)
                       ;;       ;; (buffer-substring (point) (mark))
                       ;;       )
              ;; )
             )
                  (org-copy-subtree)
                  ;; (org-mark-subtree)
                  ;; (org-end-of-meta-data t)
                  ;; (buffer-substring (point) (mark))
                  
                  (with-temp-buffer
                    (org-mode)
                    (org-yank)
                    (goto-char (point-min))
                    ;; (outline-next-heading)
                    (org-demote-subtree)
                    ;; (org-map-tree 'org-demote)
                    (org-mark-subtree)
                    (org-end-of-meta-data 1)
                    (setq str
                          (buffer-substring (point) (mark))
                          ))
                  str
                  )
       )))

;;;

(defvar-local vvstr "hi")
