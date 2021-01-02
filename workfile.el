;;; workfile.el --- a summary
;;; elisp 
;;;; binding trials
(defun vv/afn ()
  (message "I see `c', its value is: %s" c))

(defvar c t)

(let ((a "I'm lexically bound")
      (c "I'm special and therefore dynamically bound"))
  (message "I see `a', its values is: %s" a))

;;;; makeprocess, server-eval-at
(make-process
 :name "ttt"
 :buffer (get-buffer-create "ttt")
 ;; :command '("bash" "-c" "emacs")
 :command '("bash" "-c" "emacs --daemon=orgem")
 :connection-type 'pty
 )

(async-start
 (lambda ()
   (require 'server)
   (server-eval-at "~/.emacs.d/server/hi"
                   '(progn
                      (sleep-for 5)
                      (* 99999 99999999))))
 (lambda (result) (print result)))

(server-eval-at "hi" '(emacs-pid))
;;;; timer

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "parse time: %.06f" (float-time (time-since time)))))


;;;; looping 
(type-of (cl-position 2 '(6 5 4 2 1)))

(-repeat 1000 '2)


(cl-dolist (el org-xob--objects)
  (format "obj: %s   name: %s" (car el) (cdr el)))

(loop for (k . v) in org-xob--objects
      do (format "%s || %s" k v))

(cl-loop for key in (mapcar 'car org-xob--objects)
         for value in (mapcar 'cdr org-xob--objects)
         ;; collect (cons value key)
         collect (format "%s || %s" key value)
         ;; collect key value
         )

(format "O: %s ||| N: %s" (car (car org-xob--objects)) (cdr (car org-xob--objects)))

;; this works
(cl-loop for (k . v) in org-xob--objects
         collect (format "%s || %s" k v)
         )
;;; ---
;;; org stuff 
;;;; ap's searches
(-flatten
(-non-nil
 (mapcar (lambda (file)
           (let ((case-fold-search nil))
             (with-current-buffer (forg-xob-short-titlefer-visiting file)
               (org-with-wide-buffer
                (goto-char (point-min))
                (cl-loop with regexp = (format org-heading-keyword-regexp-format "MAYBE")
                ;; (cl-loop with regexp = org-heading-regexp 
                         while (re-search-forward regexp nil t)
                         collect (nth 4 (org-heading-components)))))))
         (org-agenda-files))))


(-flatten
(-non-nil
 (mapcar (lambda (file)
           (let ((case-fold-search nil))
             (with-current-buffer (forg-xob-short-titlefer-visiting file)
               (org-with-wide-buffer
                (goto-char (point-min))
                ;; (cl-loop with regexp = (format org-heading-keyword-regexp-format "MAYBE")
                (cl-loop with regexp = org-heading-regexp 
                         while (re-search-forward regexp nil t)
                         collect (nth 4 (org-heading-components)))))))
         (org-agenda-files))))



(lambda ()
  (let ((case-fold-search nil))
    (with-current-buffer (current-buffer) 
      (org-with-wide-buffer
       (goto-char (point-min))
       (cl-loop with regexp = (format org-heading-keyword-regexp-format "node")
       ;; (cl-loop with regexp = (formate org-heading-regexp)
       ;; (cl-loop with regexp = org-heading-regexp 
                while (re-search-forward regexp nil t)
                collect (nth 5 (org-heading-components)))))))

(-non-nil
(org-with-wide-buffer
 (goto-char (point-min))
 ;; (cl-loop with regexp = (format org-heading-keyword-regexp-format "node") 
 (cl-loop with regexp = org-heading-regexp
          while (re-search-forward regexp nil t)
          collect (nth 5 (org-heading-components)))))

 ;; ** meebee :node:source:

;;;; load/save ast?
(defun vv-save-ast ()
  (interactive)
  (let ((ast (org-element-parse-buffer)))
    (with-temp-file "~/Documents/1TEXT/webb"
      ;; (prin1 ast (current-buffer)))))
      (prin1 (symbol-value ast) (current-buffer)))))

(defun vv-save-ast ()
  (interactive)
  (let ((ast (org-element-parse-buffer)))
    (with-temp-file "~/Documents/1TEXT/webb"
      ;; (prin1 ast (current-buffer)))))
      (prin1 (symbol-value ast) (current-buffer)))))

;; (setq org-ast (with-temp-buffer
;;                 (insert-file-contents "~/Playground/elisp/orgfile.org")
;;                 (org-mode)
;;                 (org-element-parse-buffer))


;;;; logging & org stuff 
(org-log-beginning)
(org-add-note)
(org-add-log-note)
(org-element-at-point)

(org-element-map (org-element-parse-buffer) 'LOGBOOK (lambda (el) ((let (nm (org-element-property :drawer-name "LOGBOOK"))
                                                                     (print el)))))
(org-map-entries)
(org-log-into-drawer)
(org-log-beginning t)
(org-clock-get-clock-string)
(org-clock-goto)
(org-clock-drawer-name)

;;;; parse ast fns

(measure-time (org-element-parse-buffer))
(measure-time (setq vv-ast (org-element-parse-buffer)))

(setq vv-ast nil)

(defun vv-get-ast ()
  (interactive)
  (setq vv-ast (org-element-parse-buffer)))


(defun vv-parse-agenda-file (filename)
  (org-element-parse-buffer
   (switch-to-buffer
    (org-get-agenda-file-buffer filename))))

(defun vv-parse-all-agenda ()
  (interactive)
  (save-window-excursion
    ;; (measure-time)
    (setq vv-ast
          (mapcar 'vv-parse-agenda-file
                  (org-agenda-files)))))

(defun vv-parse-all-async ()
  (interactive)
  (save-window-excursion
    (async-start-process 'emacs
     (lambda ()
       (require 'org-mode)
       (mapcar '(progn
                  (org-element-parse-buffer
                   (switch-to-buffer
                    (org-get-agenda-file-buffer filename))))
        (org-agenda-files)))
     (lambda (x) (setq vv-ast x)))))

(defun vv-parse-all-async ()
  (interactive)
  (async-start
   (vv-parse-all-agenda)
   'ignore))

;;;; org-id , hashing 

;; not relevant to UUID
;; (org-id-decode)
;; (org-id-decode (org-id-get-create))
;; (setq org-id-method 'uuid)

;; maybe later
;; org-id-extra-files ;; Its value is ‘org-agenda-text-search-extra-files’

(setq vv/hm (hash-table-))
(let ((org-id-locations ')))

(org-id-open "5fc3aafe-fa83-4ec4-9db3-12e703d31bb2")
(hash-table-keys org-id-locations)
(hash-table-count org-id-locations)
(hash-table-values org-id-locations)


(dolist (elt (hash-table-keys org-id-locations))
  (org-id-open elt)
  (message
   (org-entry-get nil "ITEM" nil)))

;;; tree stuff
;;;; 1st 

;; respect contents pasting in to 
(org-insert-heading-respect-content t)
(org-insert-heading-respect-content)

;; get headings
(outline-next-heading)
(org-next-visible-heading)


;; Call FUN for every heading underneath the current one.
(org-map-tree FUN)

;; great way to get empty subtree
(kill-new)
(let ((str))
  (org-map-tree (lambda ()
                  (setq str (concat str 
                                    (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))))
  str)

;; org-flag fns do hide/show

;; all stuff before first subheading. could be good except the way I'm doing it now.  
(org-copy-subtree nil nil nil 'nosubtrees)

;; just the text
(org-map-tree (lambda ()
                (print (nth 4 (org-heading-components)))))

;; gets object stuff
(org-map-tree (lambda ()
                (print (org-get-heading nil nil nil nil))))

;; Call FUNC at each headline selected by MATCH in SCOPE.
(org-map-entries)
;; get all headings in scope
(org-map-entries (lambda ()
                   (print (nth 4 (org-heading-components)))) nil nil nil)
;; org-get-heading

(org-forward-heading-same-level)


;;;; manipulate subtrees

;; USE THESE

;; Mark the current subtree. This puts point at the start of the current subtree, and mark at the end.  
(org-mark-subtree)

(org-mark-element)

;; mark contents
(progn 
  (org-mark-subtree)
  (org-end-of-meta-data t))

(progn 
  (org-mark-subtree)
  (org-end-of-meta-data t)
  (copy-region-as-kill nil nil 'REGION))

(insert (car kill-ring-yank-pointer))

(set-marker (mark-marker) (- (mark) 1))
(set-marker (mark-marker) 20)

;; Delete the text between START and END.
(delete-region START END)


;; interesting
(org-mode-restart)

(org-copy-subtree)
(org-paste-subtree)		
;; --- 

;; also
;; (substring) ;; meant for string arguments
(buffer-substring-no-properties)

;; tryout substring. bad on weblog
(buffer-substring (point-min) (point-max))
(buffer-substring-no-properties (point-min) (point-max))

;; can also use this
(org-copy-subtree nil 'CUT)
(org-cut-subtree)

;; a variable used in addition to kill ring
(org-subtree-clip) 

;; also useful, marks rel to beg
  ;; Check markers in region.
  ;; If these markers are between BEG and END, record their position relative
  ;; to BEG, so that after moving the block of text, we can put the markers back
  ;; into place.
(org-save-markers-in-region beg end)
(org-reinstall-markers-in-region)

;; "Save the visible outline headers between BEG and END to the kill ring.
;; Text shown between the headers isn't copied.  Two newlines are
;; inserted between saved headers.  Yanking the result may be a
;; convenient way to make a table of contents of the buffer."
(outline-headers-as-kill beg end)

;;;; use this for node with summary 
(require 'subr-x) ;; for when-let

;; OR this. 
(org-copy-subtree nil nil nil 'nosubtrees)

;; looks excessive but works. 
(defun vv-get-headline-with-text ()
  "Return a list with the headline text of the top-level headline for point as first element and the section text as second element."
    (interactive)
    (kill-new
     (org-element-interpret-data
      (when-let ((data (org-element-parse-buffer 'greater-elements)) ;; sparse parsing...
                 (headline (org-element-map
                               data
                               'headline
                             (lambda (el)
                               (let ((beg (org-element-property :begin el))
                                     (end (org-element-property :end el)))
                                 (and (>= (point) beg)
                                      (<= (point) end)
                                      el)))
                             nil
                             'first-match
                             'no-recursion))
                 (headline-text (org-element-property :raw-value headline))
                 (section (org-element-map
                              headline
                              'section
                            #'identity
                            nil
                            'first-match
                            'no-recursion))
                 (text-begin (org-element-property :contents-begin section))
                 (text-end (org-element-property :contents-end section)))
        (list headline-text (buffer-substring text-begin text-end))
        ;; (print headline-text (buffer-substring text-begin text-end))
        ))))

;; mess of testing
;; (save-excursion 
;;   (save-restriction
    ;; (org-narrow-to-subtree) 
    ;; (buffer-substring ())
    ;; (org-end-of-meta-data 'full)
    ;; (print (org-element-at-point))
    ;; (org-element-at-point)
    ;; (let ((p (org-element-at-point)))
      ;; (org-element-interpret-data)
      ;; (buffer-substring-no-properties (org-element-property :contents-begin p)
      ;;                                 (org-element-property :contents-end p)))
    ;; (org-element-property :cont (org-element-at-point))
    ;; (org-element-interpret-data 
    ;;  (org-element-at-point))
    ;; (org-element-map
    ;;     (org-element-at-point)
    ;;     'paragraph
    ;;   ;; #'identity
    ;;   (lambda (el) (print (org-element-interpret-data el)))
    ;;   nil
    ;;   'first-match
    ;;   'no-recursion
    ;;   )
    ;; ))
(progn 
  (org-goto-first-child)
  (previous-line)
  (move-end-of-line nil))

(defun get-headline-with-text ()
  "Return a list consisting of the heading title and the
first section of text (before the next heading) at point."
  (interactive)
  (print 
   (save-excursion
     (save-restriction
       (widen)
       ;; (ignore-errors (outline-up-heading 1))
       (let* ((elt (org-element-at-point))
              (title (org-element-property :title elt))
              (beg (progn (org-end-of-meta-data t) (point)))
              (end (progn (outline-next-visible-heading 1) (point))))
         (list title (buffer-substring-no-properties beg end)))))))

(defun vv-h-s ()
  "Copy to kill ring the heading and the first paragraph of text."
  (interactive)
  (kill-new
   (org-element-interpret-data 
    (let* ((data (org-element-context 'greater-elements))
           (headline (org-element-map
                         data
                         'headline
                       (lambda (el)
                         (let ((beg (org-element-property :begin el))
                               (end (org-element-property :end el)))
                           (and (>= (point) beg)
                                (<= (point) end)
                                el)))
                       nil
                       'first-match
                       'no-recursion))
           (section (org-element-map
                        headline
                        'section
                      #'identity
                      nil
                      'first-match
                      'no-recursion)))
      ;; TODO
      (list )))))

;;;; get subheadings
(defun vv/heading-children ()
  "Return list of child headings of heading at point."
  (interactive)
  (print 
   (org-with-wide-buffer
    (when (org-goto-first-child)
      (cl-loop collect (org-get-heading t t)
               while (outline-get-next-sibling))))))

(defun org-xob-to-node-tree ()
  "Show node top level contents, only show subheading headlines."
  (interactive)
  (org-end-of-meta-data 'full)
  (let ((str))
    
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-map-tree (lambda ()
                        (setq str (concat str 
                                          (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position))
                                          "\n"))))
        ))
    (org-end-of-subtree)
    (next-line)
    (insert str)
    ))
;;;; edit stuff?

(let* ((data (org-element-parse-buffer)))
  (org-element-map data 'headline
    (lambda (el)
      (when (equal
             (car-safe (org-element-property :title el))
             "Accessors")
        (setf (nthcdr 2 el) "test") ;; Here we remove the contents from this headline.
        )))
  (org-element-interpret-data data))

(org-element-map (org-element-at-point) 'headline
  (lambda (el)
    (setf (nthcdr 2 el) "test")))



(org-element-interpret-data (setf (nthcdr 2 (org-element-at-point)) "test"))
;;;; delete subtree contents

;; clean version
(defun vv-vv ()
  "delete contents of every child heading."
  (interactive) 
  (save-excursion
    (save-restriction 
      (org-narrow-to-subtree)
      (outline-show-all)
      (outline-next-heading)
      (while
          (progn 
            (save-excursion 
              (org-mark-subtree)
              (org-end-of-meta-data t)
              (call-interactively #'delete-region)
              (org-back-to-heading t))
            (outline-get-next-sibling)))))
	(org-back-to-heading t))


;; split up

;; (vv-vv #'org-xob-to-heading)
;; (puthash "5fc3aafe-fa83-4ec4-9db3-12e703d31bb2" 19 org-xob--id-node)
;; (remhash "5fc3aafe-fa83-4ec4-9db3-12e703d31bb2" org-xob--id-node)

(org-xob--map-source #'org-xob-to-heading)

(defun org-xob--is-source-p (&optional ID)
  t)
(defun org-xob--tree-delete-contents ()
  "delete contents of every child heading."
  (interactive) 
  (save-excursion
    (save-restriction 
      (org-narrow-to-subtree)
      (outline-show-all)
      (outline-next-heading)
      (while
          (progn 
            (save-excursion 
              (org-mark-subtree)
              (org-end-of-meta-data t)
              (call-interactively #'delete-region)
              (org-back-to-heading t))
            (outline-get-next-sibling)))))
	(org-back-to-heading t))

;; rough func
;; delete contents
(defun vv-vv ()
  "delete contents of every child heading."
  (interactive) 
  ;; (save-restriction)
	;; (org-back-to-heading t)
	;; (org-narrow-to-subtree)
  (save-restriction 
  (save-excursion
      ;; (org-with-wide-buffer)
      (org-narrow-to-subtree)
      (outline-show-all)
      (outline-next-heading)
      (while
          (progn 
            (save-excursion 
              (org-mark-subtree)
              (org-end-of-meta-data t)
              ;; (set-marker (mark-marker) (- (mark) 1))
              (call-interactively #'delete-region)
              ;; (delete-region (region-beginning) (region-end))
              ;; (open-line 1)
              ;; (newline)
              (org-back-to-heading t))
            (outline-get-next-sibling)
            )
        )))
  ;; (outline-up-heading)
	(org-back-to-heading t)
  ;; (org-hide-entry)
  ;; (outline-hide-body)
  ;; (outline-show-children 1)
  )

;; tryouts
(progn 
  (delete-region 
   (progn 
     (org-end-of-meta-data t)
     (point))
   (org-entry-end-position)))

(lambda () 
  (save-excursion 
    (org-mark-subtree)
    (org-end-of-meta-data t)
    (call-interactively #'delete-region)))
;;;; build kb source tree
(defun org-xob--build-source (source items)
  "New way to make source tree. Source is the heading to populate.
Items is a list of org-ids."
  ;; TODO assuming on parent header
  (if (org-xob--is-source-p)
      (progn 
        (dolist (el items nil)
          ()))))

;; (progn
;;   (goto-char (point-max))
;;   (org-insert-heading '(4) 'invisible-ok 'TOP)
;;   (insert org-xob-short-title)

(defun org-xob--source-add-item (ID)
  "Appends a single entry to the end of the source subtree.
Assumes point is on the source heading."
  (let ((title (gethash ID org-xob--id-node)))
    (if title 
        (save-excursion 
          (org-insert-subheading '(4))
          ;; alt (org-edit-headline (org-xob--ID-title ID))
          (insert title)
          (org-entry-put (point) "PID" ID)
          ;; needed?
          (org-id-get-create 'FORCE))
      (message "not a valid knowledge base ID: %s" ID))))

;; (outline-up-heading)

(defun org-xob-kb)
(defun org-xob-kb--add-item ())

;;; accessors
    :PROPERTIES:
    :ID: 46025ffd-090d-4a2b-8216-720a60e8f3d5
    :END:

(setq vv-contents (org-element-contents vv-ast))

(length vv-contents)
(length (car vv-contents))
(car (car vv-contents))
(org-element-map vv-contents 'headline
  (lambda (x)
    (print (eq (org-element-property :parent x) "Setters"))))

;; (org-element-interpret-data '(("ID" . "idee") ("ITEM" . "heading")))


#("**** [.] search: internet global
" 5 8 (org-category "org-xob" face (:height 70) wrap-prefix #("******* " 0 3 (face org-indent) 3 8 (face org-indent)) line-prefix #("***" 0 3 (face org-indent)) fontified t))


"* head
:PROPERTIES:
:ID: ,(org-id-get-create)
:CREATED:
:ORIGIN:
:LAST_VERSION:
:TAGS:
:END:"


(defmacro insert-new-node (name)
  (interactive)
  `(let ((header
         "* head
:PROPERTIES:
:ID: ,(org-id-get-create)
:CREATED:
:ORIGIN:
:LAST_VERSION:
:TAGS:
:END:"
         ))
    (print header))
  )


(insert-new-node head)


(defmacro iii-new-node (name)
  (interactive)
  `(let ((header
          ,(concat "*"
                   name
                   ":PROPERTIES:"
                   ":ID:"
                   ,(org-id-get-create)
                   ":CREATED:"
                   ,(org-time-stamp-inactive '(16))
                   ":ORIGIN:"
                   ":LAST_VERSION:"
                   ":TAGS:"
                   ":END:")
          ))
     (print header)))


(defun iii-new-node (name)
  (interactive)
  (let* ((now (format-time-string "%Y-%m-%d %a %k:%M" (current-time)))
        (header
          (concat "* "
                   name "\n"
                   ":PROPERTIES:\n"
                   ":ID: "
                   (org-id-get-create 'FORCE) "\n"
                   ":CREATED: "
                   "[" now "]" "\n"
                   ":ORIGIN:\n"
                   ":LAST_VERSION:\n"
                   ":TAGS:\n"
                   ":END:\n")
          ))
    (kill-new header)))
(iii-new-node "head")

(format-time-string "%Y-%m-%d %a %H"  (current-time))

(org-element-property :level (org-element-at-point))

;;; linking
;;;; 1st linking
(setq org-id-link-to-org-use-id t)

(progn (setq vv-b (org-id-store-link)) (message vv-b))

(org-element-property :ID (org-element-at-point))
(org-element-property :CPARENTS (org-element-at-point))

;;;; backlinks

;; this works
(defun vv-goto-bl ()
  (interactive) 
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (if (equal (org-element-property
                  :drawer-name (cadr (org-element-lineage link)))
                 "BACKLINKS")
          (org-element-property :path link)))))

(setq vvb (vv-goto-bl))

;; inverse, links not in backlinks
(defun vv-goto-bl ()
  (interactive) 
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (if (not (equal (org-element-property
                       :drawer-name (cadr (org-element-lineage link)))
                      "BACKLINKS"))
          (org-element-property :path link)))))

;; (print (org-element-property :drawer-name (cadr  (org-element-lineage link))))
;; (equal (org-element-property :drawer-name (car (org-element-lineage link)))) "BACKLINKS")
;; (equal (org-element-property :drawer-name adrawer) "BACKLINKS")

;; (setq here (org-element-property :contents-begin adrawer)

(org-element-context) 
(org-element-at-point)

(equal (org-element-property :drawer-name (org-element-at-point)) "BACKLINKS")
(equal (org-element-property :drawer-name (car (org-element-lineage (org-element-at-point)))) "BACKLINKS")

(org-element-property :drawer-name (car (org-element-lineage (org-element-at-point))))

(org-element-lineage (org-element-at-point))

(org-element-property :parent 
(org-element-property :parent 
 (org-element-context))) 

;;; ---
;;; get node
;;;; main get node  
;;;; testing version for reference
;; (setq org-xob-today nil)

;; (defun org-xob-get-node ()
;;   (interactive)
;;   (when (not org-xob-on-p)
;;     (org-xob-start))
;;   (unless org-xob-today
;;     (setq org-xob-today (org-xob--capture "today")))
;;   ;; Get node by title, or create new one 
;;   (helm :buffer "*xob get node*"
;;         :sources (helm-build-sync-source "vv-sss"
;;                    :candidates (lambda ()
;;                                  (let* ((candidates (hash-table-keys org-id-locations))
;;                                         ;; (let* ((cans (hash-table-keys org-xob--title-id))
;;                                         )
;;                                    (cons helm-input candidates)))
;;                    :volatile t
;;                    ;; :action (lambda (title) (let ((ID (gethash title org-xob--title-id)))
;;                    :action (lambda (title) (let ((ID (gethash title org-id-locations)))
;;                                            (unless ID
;;                                              (setq ID (org-xob--capture title)))
;;                                            (org-xob--activate-node ID))))))

;;;; alternative to get contents, used in activate-node fn
;; (contents (save-mark-and-excursion (progn 
;;                                      (marker-buffer m)
;;                                      (goto-char m)
;;                                      (move-marker m nil)
;;                                      (org-copy-subtree)))))

;;;; misc
;; (cl-pushnew org-xob-today (node-backlinks
;;                                 (org-xob--id-node ID)))

(setq title "bee")
(setq title "day")
(setq title "log personal")
(setq title "log")

;; old idea
;; (defun org-xob--find-node ()
;;   ;;
;;   ;; fuzzy search node titles
;;   (let ((pos (org-id-find id)))
;;     ;; get node (header+contents) at pos (filename . possible))
;;     ))

;;;; xob edit node

;; A org-tree-to-indirect-buffer 
(org-tree-to-indirect-buffer &optional ARG)
(org-tree-to-indirect-buffer)

;;;; live sync V2

;; hello there

(add-hook 'after-change-functions #'vv-test-edits nil t)
(remove-hook 'after-change-functions #'vv-test-edits)
before-change-functions

(defun vv-test-edits (beg end len)
  (interactive)
  (print (buffer-substring-no-properties beg end)))

(setq clip nil)
(defun vv-test-edits (beg end len)
  (interactive)
  (if (eq len 0)
      (progn
        (unless clip
          (setq clip beg)))
    (progn
      (print (buffer-substring-no-properties clip end))
      (setq clip nil))
    ))
;; (prin1-to-string (list beg end len (buffer-substring-no-properties beg end))))

;; this is also promising. in subr
(text-clone-create)

;; ??
inhibit-modification-hooks


;; hello
;; hello
;;; helm | hashing
;;;; scratch 
(defun vv/helm-try ()
  (interactive)
  (helm :sources (helm-build-async-source "async-src"
                   :candidates-process
                   (lambda ()
                     (start-process "ls" nil "ls"))))
  :buffer "*helm vv/asynce-try")

;; (setq helm-candidate-number-limit 500)
;; (setq helm-candidate-number-limit nil)

;; (setq vv-sources '((one . "one") (two . "two") (three . "three")))
;; (type-of vv-sources)
;; (setq action nil)

;;;; scratch 2nd
(setq vv/helmsource
      '((name . "helm source")
        (candidates . (1 2 3 4))
        (candidate-transformer #'vv/hl-xformer)
        (action . (lambda (candidate)
                    (message-box "%s" candidate)))))

(defun vv/hl-xformer (cans)
  (let ((prefix "[?]"))
    (cons 'prefix 'cans)))

(helm :sources '(vv/helmsource)
      ;; :filtered-candidate-transformer #'vv/hl-xformer
      )

(setq vv/data '(("John" . "john@email.com")
                ("Jim" . "jim@email.com")
                ("Jane" . "jane@email.com")
                ("Jill" . "jill@email.com")))

(setq vv/helmsource
      '((name . "helmy")
        (candidates . vv/data)
        (action . (lambda (candidate)
                    (helm-marked-candidates)))))
(defun vv/hsie ()
  (interactive)
  (insert (mapconcat 'identity
                     (helm :sources '(vv/helmsource)) ",")))

(defun vv/cne ()
  (interactive)
  (message "hi"))

(require 'org-roam)
(org-roam)


;;;; helm hash sources
;; https://emacs.stackexchange.com/questions/52552/helm-source-from-a-hash-table
(let ((utf8-hash-table (ucs-names)))
  (helm :sources
        `((name . "Unicode character by name")
          (candidates . ,(hash-table-keys utf8-hash-table))
          (action . (lambda (key) (insert (gethash key ,utf8-hash-table)))))))
;;;; correct
(helm :sources (helm-build-sync-source "vv-sss"
                 :candidates (lambda ()
                               (let* ((cans (hash-table-keys org-id-locations))
                               ;; (let* ((cans (hash-table-keys org-xob--title-id))
                                      )
                                 (cons helm-input cans)))
                 ;; :filtered-candidate-transformer #'(lambda (cans source)
                 ;;                                     (cons "[?] " cans))
                 :volatile t
                 ;; :action (list (cons "new node" (lambda (key) (org-xob--capture key))))
                 :action (lambda (key) (let ((ID (gethash key org-id-locations)))
                 ;; :action (lambda (key) (let ((ID (gethash key org-xob--title-id)))
                                         (if ID
                                             (org-xob--get-entry ID)
                                           (org-xob--capture key)))))
      :buffer "*exo get node*")

(helm :sources
      `((name . "org-ID-locations")
        (candidates . ,(hash-table-keys org-id-locations))
        (action . (lambda (key) (gethash key ,org-id-locations)))))


(helm :sources
      `((name . "org-ID-locations")
        (candidates . ,(cons helm-input (hash-table-keys org-id-locations)))
        (action . (lambda (key) (let ((id (gethash key ,org-id-locations)))
                                  ;; (if id
                                  ;;     (org-xob--get-entry id))
                                  (org-xob--capture key))))))


(helm :sources
      `((name . "Exobrain Nodes")
        (candidates . ,(hash-table-keys org-xob--title-id))
        (action . (lambda (key) (let ((ID (gethash key ,org-xob--title-id)))
                                  (if ,ID
                                      (org-xob--get-entry ,ID)
                                    (org-xob--capture helm-input)))))))

;;;; hash table 
;; create empty table
(setq org-xob--table-size 10000)
(setq org-xob--title-id (make-hash-table
                              :test 'equal
                              :size org-xob--table-size))

(setq org-xob--id-node (make-hash-table
                         :test 'equal
                         :size org-xob--table-size))
;; store node title and id NewNode
(puthash title id org-xob--title-id)

;; get the node location from title
(gethash (gethash title org-xob--title-id) org-id-locations)

;;;;; rough
;; (maphash #'vv/is-exobrain-node 'org-id-locations)

;; (setq vv/ttls (list))
;; (defun vv/nodetitles (k v)
;;   (org-id-goto k)
;;   ;; (add-to-list 'vv/ttls 
;;   ;;              ;; (org-entry-get nil "ITEM")
;;   ;;              (nth 4 (org-heading-components))
;;   ;;              )
;;   (push (nth 4 (org-heading-components)) vv/ttls)
;;   )
;; (maphash #'vv/nodetitles org-id-locations)
;; (length vv/ttls)

;;;; load/save hashtable
;; https://stackoverflow.com/questions/11745097/serializing-an-emacs-lisp-hash-to-file
(featurep 'hashtable-print-readable)  ;; check if this feature is true, then can use prin1


;; https://stackoverflow.com/questions/36193866/idiomatic-way-serialization-in-emacs-lisp
;; write
(defun org-xob--save-object (file data)
  (with-temp-file file
    (prin1 data (current-buffer))))

;; Read from file:
(defun org-xob--load-object (file symbol)
  (when (boundp symbol)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (set symbol (read (current-buffer))))))

;; Call to write:
(my-write "test.txt" org-id-locations)

;; Call to read
(my-read "test.txt" 'my-emacs-version)
;;; ---
;;; Versioning
;;;; ediff saving
(ediff-save-buffer '4)
;;;; shell calls
;;;; no
(shell-command-to-string
 (format "wdiff -w \"[[delete:\" -x \"]]\" -y \"[[insert:\"  -z \"]]\" <(git show efc4752:2018-10-27.org) <(echo \"%s\")"
         (with-current-buffer
             (find-file-noselect "2018-10-27.org")
           (shell-quote-argument
            (buffer-substring-no-properties (point-min) (point-max))))))


(shell-command-to-string
 (format "diff -u %s %s"
         (shell-quote-argument "mm.org")
         (shell-quote-argument "mm2.org")))

;; (shell-command "ls | grep 3")
(shell-command-to-string
 (format "xdelta3"
 "-ecAS %s %s | xargs xdelta3 -d <(echo \"%s\")"
         (shell-quote-argument "mm.org")
         (shell-quote-argument "mm2.org")
         (shell-quote-argument "mm.org")
         ))
         
;; (shell-command-to-string
;;  (format "xdelta3 -ecAS %s %s | xargs xdelta3 -d <(echo \"%s\")"
;;          (shell-quote-argument "mm.org")
;;          (shell-quote-argument "mm2.org")
;;          (shell-quote-argument "mm.org")
;;          ))
;;;; yes. works:
(shell-command-to-string
 (concat "xdelta3 -cd -s " "mm.org" " <( xdelta3 -ecAS -s mm.org mm2.org)"))

(shell-command-to-string
 (concat "xdelta3 -cd -s " "mm2.org" " <( xdelta3 -ecAS -s mm2.org mm.org)"))

;;;; proper cli strings
;;;;; round trip test
(shell-command-to-string
 (combine-and-quote-strings
  `("xdelta3"
    ,(shell-quote-argument "-cd")
    "-s"
    "mm.org"
    "<("
    "xdelta3"
    ,(shell-quote-argument "-ecAS")
    "-s"
    "mm.org"
    "mm2.org"
    ")"
    )))

;;;;; encode

;; order is "now-node", then "last-node"

(shell-command-to-string
 (combine-and-quote-strings
  `("xdelta3"
    ,(shell-quote-argument "-ecAS")
    "-s"
    ,(shell-quote-argument
      ;; "<(echo \"this that\")")
      "<(echo boo)")
    ,(shell-quote-argument
      ;; "<(echo \"this though that\")")
    "<(echo booooooo)")
    )))

(shell-command-to-string
(concat
 "xdelta3 "
 "-ecAS "
 "-s "
 "<(echo boo) "
 "<(echo boooooooooooooo)"
 ))

(setq vv/s1 "hello world")
(setq vv/s2 "hello NEW NEW world")
(setq vv/diff
      (shell-command-to-string
       (combine-and-quote-strings
        `(
          "xdelta3"
          "-ecAS"
          "-s"
          "<("
          "echo"
          ;; "boo"
          ,vv/s1
          ")"
          "<("
          "echo"
          ;; "boooooooooooooo"
          ,vv/s2
          ")"
          )
        )))

;; to diff file
(shell-command
 (combine-and-quote-strings
  `(
    "xdelta3"
    "-eAS"
    "-s"
    "<("
    "echo"
    ,vv/s1
    ")"
    "<("
    "echo"
    ,vv/s2
    ")"
    ,(org-xob--diff-filename 'node)
    )
  ))

(defun org-xob--diff-node (node)
  (shell-command
   (combine-and-quote-strings
    `("xdelta3"
      "-eAS"
      "-s"
      "<("
      "echo"
      ,vv/s1
      ")"
      "<("
      "echo"
      ,vv/s2
      ")"
      ,(org-xob--diff-filename 'node)
      )
    )))
;;;;; decode
(shell-command-to-string
 (combine-and-quote-strings
  `("xdelta3"
    ,(shell-quote-argument "-cd")
    "-s"
    "now-node"
    "diff"
    )))

;;;;; NO, won't bother. roundtrip 2
(shell-command-to-string
 (combine-and-quote-strings
  `(
    "xdelta3"
    ,(shell-quote-argument "-cd")
    "-s"
    "<("
    "echo"
    ;; "boo"
    ,vv/s1
    ")"
    "<("
    "echo"
    ,(shell-command-to-string
     (combine-and-quote-strings
      `(
        "xdelta3"
        "-ecAS"
        "-s"
        "<("
        "echo"
        ;; "boo"
        ,vv/s1
        ")"
        "<("
        "echo"
        ;; "boooooooooooooo"
        ,vv/s2
        ")"
        )
      )))))
;;;; getting unsynced nodes
 (setq vv/ll '(1 2 3 4 4 4 4))
(remove-if (lambda (x) (eq x 4)) vv/ll)

(remove-if (lambda (buf) (with-current-buffer buf (not org-xob))) (buffer-list))
(remove-if (lambda (buf) (with-current-buffer buf org-xob)) (buffer-list))

(remove-if  (with-current-buffer buf org-xob) (buffer-list))

(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when org-xob
      (when (eq major-mode 'org-mode)
        (print buf)))
    ))

(org-xob--active-buffers)

(remove-if-not (lambda (buf) (buffer-modified-p buf)) (org-xob--active-buffers))
(remove-if-not #'buffer-modified-p (org-xob--active-buffers))

;;;; try out dash.el
(-> '(1 2 3)
    (append '(8 13))
    (-slice 1 -1))

(defun square (n) (* n n))
(->> '(1 2 3) (-map 'square) (-reduce '+))
(->> '(1 2 3))

(-> '(1 2 3)
    (append '(8 13)))

(-> '(1 2 3)
    (append '(8 13) '(9 9)))
;; (1 2 3 8 13 9 9)

(->> '(1 2 3)
    (append '(8 13) '(9 9)))
;; (8 13 9 9 1 2 3)

(--> '(1 2 3)
    (append '(8 13) it '(9 9)))
;; (8 13 1 2 3 9 9)

;;; ---
;;; node structs and hashtable
(cl-defstruct node title type backlinks)
;; (setq vv/ns (make-node :title "meee" :backlinks (list)))
;; nice to know, not using it 
;; (setf (node-backlinks vv/ns) (append '(a)))
;;; trial #2 xob state : alternate use struct for state
(cl-defstruct xob-state kb-count kb-current kb-files t-id-table-fn id-n-table-fn)
(setq xob (make-xob-state :kb-count 0 :t-id-table-fn "title-id-table" :id-n-table-fn "id-node-table"))

(defun xob-new-file ()
  (interactive)
  (let ((filename (concat 
                   org-xob--KB-filename-prefix
                   (format "%03d" (xob-state-kb-count xob))
                   ".org")))
    (with-temp-buffer
      (write-file filename))
    (cl-pushnew filename (xob-state-kb-files xob))
    (setf (xob-state-kb-current xob) filename)
    (setf (xob-state-kb-count xob) (+ 1))))

(xob-state-id-n-table-fn xob)

(setf (xob-state-kb-current xob) "KB-file-000.org")

(org-xob--save-object "xob_state" xob)
(prin1 xob)
(symbol-value xob)
(prin1 org-xob--title-id)

;; need separate load/save for struct
;; save struct, no symbol-value
(with-temp-file "xob_state"
  (prin1 xob (current-buffer)))

;; load struct use setq
(setq xobii nil)
(with-temp-buffer
  (insert-file-contents "xob_state")
  (goto-char (point-min))
  (setq xobii (read (current-buffer))))

(xob-state-kb-files xobii)
(xob-state-kb-count xobii)
(xob-state-id-n-table-fn xobii)

;;;; old
(defun org-xob--new-KB-file ()
  "Create new KB file for next node in the brain."
  (let ((filename (concat org-xob-path
                          org-xob--KB-filename-prefix
                          (format "%03d" (length (directory-files org-xob-path nil org-xob--KB-filename-prefix)))
                          ".org")))
    (with-temp-buffer
      (write-file filename))
    (push filename org-xob--KB-files)))

;;; ---
;;; org-capture
;;;; other capture hooks

;; (remove-hook 'org-capture-mode-hook #'org-xob--capture-create-id)
org-capture-mode-hook ;; after entering or leaving
org-capture-prepare-finalize-hook ;; capture buffer narrowed
org-capture-before-finalize-hook  ;; capture buffer widened
org-capture-after-finalize-hook ;; done. for closing stuff

;;;; old capture fn for hook
;; (defun org-xob--new-node (title)
;; (defun org-xob--new-node ()
;;   (when (org-capture-get :exobrain-node)
;;     (org-id-get-create)
;;     (setq temp-ID (org-id-get))
;;     ;; (push (nth 4 (org-heading-components)) title)
;;     (org-entry-put (point) "VID" (org-capture-get :vid))
;;     (org-entry-put (point) "CREATED" (concat "["
;;                                              (format-time-string "%F %a %R")
;;                                              "]"))
;;     (org-entry-put (point) "TYPE" (org-capture-get :ntype)))
;;   )

;;;; main capture fn

(defun org-xob--capture (title)
  (let* ((org-capture-templates org-xob--templates)
         ID)
    ;; TODO test
    (if (member 'title 'org-xob--auto-types)
        (org-capture nil title)
      (org-capture))
    ID))

(defun org-xob--new-node ()
  (when (org-capture-get :exobrain-node)
    (let ((ID (org-id-get-create))
          (type (org-capture-get :ntype))
          node)
      ;; (push (nth 4 (org-heading-components)) title)
      (org-entry-put (point) "VID" (org-capture-get :vid))
      (org-entry-put (point) "CREATED" (concat "["
                                               (format-time-string "%F %a %R")
                                               "]"))
      (org-entry-put (point) "TYPE" type)
      (if (org-capture-get :todo) (org-todo))
      (setq node (make-node :title title
                            :type type 
                            :backlinks (list)))
      (puthash title ID org-xob--title-id)
      (puthash ID node org-xob--id-node)
      ID)))

(add-hook 'org-capture-mode-hook #'org-xob--new-node)

(defvar org-xob--auto-types '(("day" . a.day)
                              ("ad" . a.day)
                              ("session" . a.session)
                              ("project" . a.project)
                              ("log" . a.log)
                              ("log personal" . a.log.life)
                              ("log it tools" . a.log.it-tools)
                              ("log tools" . a.log.tools)
                              ("log project" . a.log.project)
                              ("article" . n.bib.article)
                              ("webpage" . n.bib.web)
                              ("fast" . n.n)
                              ("topic" . n.topic)
                              ))
(defvar org-xob--te)

(setq org-xob--templates
      '(("f" "fast" entry (file org-xob--KB-file)
         "* %^{description} \n:BACKLINKS:\n:END:\n\n%?"
         :exobrain-node t
         :ntype "node"
         :vid "0"
         ;; :immediate-finish t
         :empty-lines-after 1)
        ("ad" "today" entry (file org-xob--KB-file)
         "* %u \n:BACKLINKS:\n:END:\n\n"
         :exobrain-node t
         :immediate-finish t
         :ntype "context.day")
        ("t" "todoID" entry (file "KB-file-000.org")
         "* %^{description} \n:BACKLINKS:\n:END:\n\n%?"
         :exobrain-node t
         :todo t
         :ntype "a.todo"
         )
        ))

;;;;; capture scraps
(setq org-xob--KB-file "KB-file-000.org")
(org-xob--capture "ad")
(org-xob--capture "f")
(org-xob--capture "d")
(org-xob--capture "r")
(org-todo)
(cadr org-todo-keywords)

;; (org-capture-string "f")
(org-capture nil "lw")
;; (plist-get 'org-capture-plist :key)
;; (defun org-xob--link-nodes (ida idb)
;;   (org-id-open ida))

;; (defun org-xob--get-entry (key)
;;   (format "getting entry %s" key))

;;; var prob
(org-xob--activate-node '"5fc3aafe-fa83-4ec4-9db3-12e703d31bb2")
(org-xob--save-state)
(symbol-value (car (car org-xob--objects)))
(setq org-xob--title-id 4)

(setq org-xob--objects '((org-xob--title-id . "title-id-table")
                              (org-xob--id-node . "id-node-table")
                              (org-xob--KB-file . "current-KB-file")))


(cl-loop for (k . v) in org-xob--objects
         do (progn
              ;; (print k)
              (print (symbol-value k))
              (set k 4)
              (print v)
              ;; (setq (symbol-value k) 4)
              ))

;;; seconds timestamp
(time-convert (current-time) 'integer)
(car (time-convert (current-time) '10000))
;; (concat "[" (format-time-string "%F %a %R") "]")

;;; Workspace
;;;; dual buffers/windows
;;;; atomic window ex.
(let ((window (split-window-right)))
  (window-make-atom (window-parent window))
  (display-buffer-in-atom-window
   (get-buffer-create "*Messages*")
   `((window . ,(window-parent window)) (window-height . 5))))

(let ((window 
       (display-buffer-in-atom-window
        (get-buffer-create "*node context*")
        `((window . ,(selected-window)) (side . right))))))

;; (defun org-xob-open-sideline ()
;;   "Open context content in a side window."
;;   (interactive)
;;   ;; (org-xob-new-buffer)
;;   (let ((window (split-window-right)))
;;     ;; (window-make-atom (window-parent window))
;;     (display-buffer-in-atom-window
;;      (get-buffer-create "*node context*")
;;      `((window . ,(window-parent window)) (window-height . 5)))))

(defun vv-ssw ()
  "Open context content in a side window."
  (interactive)
  ;; (org-xob-new-buffer)
  (setq vv-sw 
         (display-buffer-in-atom-window
          (get-buffer vv-sb)
          `((window . ,(selected-window)) (side . right)))))

(defun vv-tgg ()
  (interactive)
  ;; (split-window (selected-window) nil 'right nil)
  ;; (split-window-right)
  (split-window-right-and-focus)
  ;; (delete-window)
  ;; (delete-window vv-sw)
  ;; (delete-window (window-parent vv-sw))
  (delete-other-windows)
  )

(delete-window)
(delete-other-windows)

(window-atom-root)
(window-tree)

(display-buffer 
 (window-buffer))

(setq vv-sb (get-buffer-create "*node context*"))

(defun vv-sw ()
  (interactive)
  (setq vv-sw 
        (display-buffer-in-side-window vv-sb
                                       `((side . right) (slot . 0)))))

(defun vv-tgg ()
  (interactive)
  (window-toggle-side-windows))

;; (defun vv-sw ()
;;   (interactive)
;;   (setq vv-sw 
;;         (split-window (selected-window) nil 'right nil))
;;   (with-selected-window vv-sw (display-buffer vv-sb)))

(defun org-xob-toggle-sideline ()
  "Toggles display of the contextual side window."
  (interactive)
  (if org-xob--sideline-window 
      (progn 
        (delete-window org-xob--sideline-window)
        (setq org-xob--sideline-window nil))
    (progn 
      (setq org-xob--sideline-window 
            (split-window-right))
      (select-window org-xob--sideline-window)
      (display-buffer-same-window org-xob--context-buffer nil))))


(defun vv-tgg ()
  (interactive)
  )

(org-xob--open-display "beee")
(org-xob--open-display "beeebaaaaabyyyyyyy")

;;;; possible state vars for active workspace. so far don't need this way:
;; (defvar org-xob--active-list nil
;;   "Alist of active nodes and their buffer locations.")
;; OR check for ID in buffers
;; (defun org-xob--active-p (ID)
;;   "Returns t if the node is already in the workspace."
;;   nil)
;; (defun org-xob--get-buffer (ID)
;;   nil
;;   )
;; (setq ID )


;;;; ---
;;;; old design workspace
;; (defun org-xob--active-buffers ()
;;   "Returns list of all active exobrain buffers"
;;   (remove-if-not (lambda (buf) (with-current-buffer buf org-xob)) (buffer-list)))

;; (defun org-xob--nodes-in-buffer (buff)
;;   ;; traverse headings, check if node, append ID to list
;;   nil 
;;   )

;;;; "Search buffers for org heading with ID and place point there."

(org-xob--goto-heading "3A3BD225-A186-4CC4-B900-DF10DAA31B42")
(setq vv-id "3A3BD225-A186-4CC4-B900-DF10DAA31B42")
(progn 
  (re-search-forward vv-id)
  (org-back-to-heading 'invisible-ok))
 
;; MAYBE or set marker?
;; not map, loop till found
;; TODO excessive for this version. I'm at the conext buffer already when called. 
(defun org-xob--goto-heading (ID)
  "Search buffers for org heading with ID and place point there."
  (let ((mm))
    (save-excursion
      (save-restriction
        (setq mm 
              (catch 'found 
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    ;; TODO finer than org
                    (if (eq major-mode 'org-mode)
                        (progn
                          (org-with-wide-buffer 
                           (goto-char (point-min))
                           (when (re-search-forward ID nil t)
                             (progn 
                               (org-back-to-heading 'invisible-ok)
                               (throw 'found (point-marker)))))))))))))
    (if (and (marker-buffer)
             (car mm))
        
        (progn 
          (switch-to-buffer (marker-buffer mm))
          (goto-char mm)
          t)
      nil)))




;;;; create buffer
(copy-to-buffer "testbuff" (point-min) (point-max))
(defun org-xob-open-node-buffer (item)
  (interactive)
  (with-current-buffer (get-buffer-create item)
    (unless org-xob-mode) (org-xob-mode)
    ;; insert node contents
    )
  )

;;; contexts
;;;; get node text?

;; parsing
(org-element-context)
(org-element-at-point)
;; write
(org-element-interpret-data)
;; access about thing
(org-element-type)
(org-element-property)
;; access
(org-element-map)

;; this just gets heading
(kill-new (org-element-interpret-data (org-element-contents (org-element-context))))

(defun vv-yank-el ()
  (interactive)
  (kill-new
   (org-element-interpret-data
    (org-element-map (org-element-parse-buffer 'greater-elements) 'headline
      (lambda (el)
        ;; (when (org-element-property :raw-value "Accessors" el)
        ;;   (org-element-contents el)
        ;;   (message el))))
        (org-element-contents el)
        ))
    )
   ))

(org-element-contents (org-element-at-point))

;; [[https://emacs.stackexchange.com/questions/48430/get-section-text-with-elisp][org mode - Get Section text with elisp - Emacs Stack Exchange]]  [2020-07-19 Sun 07:24]

;;;; org-heading-components stuff

(setq vv-p nil)
(length vv-p)
(org-map-entries (lambda () (push (nth 4 (org-heading-components)) vv-p)))
(org-map-entries (lambda () (push (cons (point) (nth 4 (org-heading-components))) vv-p)))
(org-map-entries (lambda () (push (point) vv-p)))

(org-entry-put (point) "NID" 
               (org-entry-get (point) "ID" nil nil))

;;;; tags
(defvar orb-xob--bl-tag "backlinks")
(defvar orb-xob--fl-tag "forlinks")
(defvar orb-xob--A-tag "A")
(defvar orb-xob--node-tag "node")

;;; [2020-12-19 Sat 09:43] 

(defun org-xob--display-source (source mainID)
  "Open a source tree for node mainID into the context buffer.
If it is already there, then refresh it. source items are shown as org headings.
source is a plist that describes the content source."
  (interactive)
  (save-window-excursion 
    (with-current-buffer org-xob--context-buffer
      (if (org-xob--goto-heading source)
          (progn
            (org-xob-refresh-source source mainID))
        (progn
          (goto-char (point-max)) ;; respecting content below is this needed?
          (org-insert-heading (4) 'invisible-ok 'TOP)

          ;; source semi specific
          (insert org-xob-short-title) ;; TODO fix: make or as arg or buf local
          (setq org-xob-backlinks-tree (org-id-get-create))
          (org-toggle-tag "KB" 'ON)
          (org-toggle-tag "backlinks" 'ON)
          ;;

          ;; specific
          ;; get-source-candidates
          (setq org-xob-node-backlinks (cons (org-xob--get-backlinks mainID)
                                             ;;

                                             (org-xob-backlinks-tree)))))
      ;; TODO modify for new way
      (org-xob-update-kb-context-at-point org-xob-node-backlinks 'headings))))

(setq vv-plist '(:name "hi"
                       :dict-entry "five"
                       :body "them"))

(plist-get vv-plist :name)
(plist-get vv-plist :body)
(plist-get vv-plist :dict-entry)

(setq-local org-xob--source-backlinks
            '(:name "backlinks"
                    :tags ("KB")
                    :title nil 
                    :ID nil
                    :PID nil
                    :func org-xob--get-backlinks
                    :items nil))

(setq-local org-xob--source-forlinks
            '(:name "forlinks"
                    :tags ("KB")
                    :title nil 
                    :ID nil
                    :PID nil
                    :func org-xob--get-forlinks
                    :items nil))

;; sharing buffer-local variables
;; (setq-local vvv 5)
;; (buffer-local-value 'vvv (get-buffer "workfile.el"))

;; seems messy
;; for name, test equal of not-equal
;; (let* ((linktype (plist-get source :name))
;;        (test (if (equal linktype "backlinks")
;;                  (lambda (x y) (equal x y))
;;                (if (equal linktype "forelinks")
;;                    (lambda (x y) (not (equal x y)))))))
;;   (org-id-goto (plist-get source :PID))
;;   (plist-put source :items
;;              (org-element-map (org-element-parse-buffer) 'link
;;                (lambda (link)
;;                  (if (funcall test (org-element-property
;;                                     :drawer-name (cadr (org-element-lineage link)))
;;                               "BACKLINKS")
;;                      (org-element-property :path link))))))
(setq target "boo")
(condition-case nil 
    (org-id-goto vvv)
    (error (message "not found")))

(condition-case nil 
    ;; (progn) 
  (print "low") 
  (print "beem")
  (error (message "eeem!")))

;; this may not needed for full node, I will ignore ID altogether, and just use PID. 
;; I may need to change where org-id is used. 
(org-id-get-create 'FORCE)
;;; try macro

(defmacro vvm (&rest body)
  ;; (declare (debug (body)))
  `(let ((func (lambda () (progn 
                            (org-xob-clear-heading)
                            (org-end-of-meta-data)
                            (insert
                             (save-excursion
                               (with-current-buffer "AcademicLog.org"
                                 (org-with-wide-buffer
                                  (org-save-outline-visibility t
                                      (outline-show-all)
                                      (goto-char (point-min))
                                      ,@body)))
                               ))))))
     (if (org-xob--is-source-p)
         (org-xob--map-source func)
       (funcall func))))

(org-end-of-meta-data 1)
;; (org-narrow-to-subtree)

(defun vvmm ()
  (interactive)
  (vvm (progn
         (outline-next-heading)
         (org-mark-subtree)
         (buffer-substring (point) (mark))
         ;; (org-end-of-meta-data t)
         ;; (buffer-substring (line-beginning-position) (line-end-position))
         )))

(defmacro vvem (&rest body)
  ;; (declare (debug (body)))
  `(let* ((str)
          (func (lambda () (progn
                            (org-xob-clear-heading)
                            ;; (goto-char (- (point) 1))
                            ;; (org-end-of-meta-data)
                            (save-excursion
                              (setq str ,@body))
                            (insert str)
                            ;; (org-back-to-heading)
                               ))))
     (if (org-xob--is-source-p)
         (org-xob--map-source func)
       (funcall func))))

(defun vve ()
  (interactive)
  (vvem (progn
          (previous-line 4)
          (concat (word-at-point) " booo"))))

(defun eev ()
  (interactive)
  (org-xob-clear-heading)
  (goto-char (- (point) 1))
  (insert "hi")
  ;; (org-back-to-heading)
  ;; (org-end-of-meta-data)
  )
;;; try nested lambdas instead of macro

(defun nln (payload)
  (let ((func (lambda () (progn 
                            (org-xob-clear-heading)
                            (org-end-of-meta-data)
                            (insert
                             (save-excursion
                               (with-current-buffer "AcademicLog.org"
                                 (org-with-wide-buffer
                                  (org-save-outline-visibility t
                                    (outline-show-all)
                                    (goto-char (point-min))
                                    (funcall payload))))
                               ))))))
     (if (org-xob--is-source-p)
         (org-xob--map-source func)
       (funcall func))))

(defun lln ()
  (interactive)
  (nln #'(lambda ()
         (progn
           (org-next-visible-heading 5)
           (next-line)
           (next-line)
           (org-mark-element)
           (set-mark (- 10 (mark)) )
           (buffer-substring (point) (mark))))))

(outline-show-subtree)
(progn
  (org-narrow-to-subtree)
  (outline-show-all)
  )

(print (org-tree-to-indirect-buffer))

(select-window (org-tree-to-indirect-buffer))

;;; proper indirect buffer call
(defun org-subtree-to-indirect-buffer ()
  (interactive)
  (let ((org-xob-short-title (concat (buffer-name) "-narrowclone")))
    (if (get-buffer org-xob-short-title)
        (kill-buffer org-xob-short-title))
    (clone-indirect-buffer-other-window org-xob-short-title t)
    (org-narrow-to-subtree)
    (switch-to-buffer org-xob-short-title)))

;; snip from old
;; (setq org-xob-node-buffer (get-buffer-create morg-xob-short-title))
;; (set-buffer org-xob-node-buffer)
;; (org-mode)

;;; old open day node - no

(unless org-xob-today
  (save-excursion 
    (condition-case nil
        (setq org-xob-today (find-file org-xob-current-log-file))
      (error "xob log file not found."))
    ;; (setq org-xob-today (get-buffer-create "org-xob-today"))
    ;; (with-current-buffer org-xob-today)
    (org-mode)
    ))

;;; crusht
    :PROPERTIES:
    :ID: 4F9789E0-2692-47A3-A040-2952CD4281CC
    :END:
(setq org-xob-path "xob/")
(org-xob--save-state)

(push "KB-file-001.org" org-xob--KB-files)
(setq org-xob-on-p nil)

(if (not org-xob-today)
    (setq org-xob-today (org-xob--capture "ad")))

(setq org-xob--log-file "")

;; ref: org-xob--auto-types '(
;;                               ("ad" . a.day)
;;                               ("as" . a.session)  								;; session
;;                               ;; ("ap" . a.project)									;; project

;;                               ("al" . a.log) 											;; log
;;                               ("all" . a.log.life)								;; log personal rundschau too
;;                               ("alit" . a.log.it-tools) 					;; log it tools
;;                               ("alt" . a.log.tools)  							;; log tools
;;                               ("lp" . a.log.project)							;; log project

;;                               ("nn" . n.n)												;; new general node 
;;                               ("nt" . n.topic)										;; kb topic
;;                               ("na" . n.bib.article)							;; bib article
;;                               ("nw" . n.bib.web)									;; bib webpage

;;                               ("tf" . t.free)											;; unbound todo	
;;                               ;; ("tp" . t.project)									;; project todo	

(setq org-xob--auto-types '("ad" "as" "al" "all" "alit" "alt" "lp" "nn" "nt" "na" "nw" "tf" "tp"))

(setq org-xob--templates
      '(("nn" "new node" entry (file org-xob--KB-file)
         "* %(eval org-xob--last-title) \n:PROPERTIES:\n:TYPE:\t\t\tn.n\n:CREATED:\t\t%U\n:MODIFIED:\t\t%U\n:END:\n:BACKLINKS:\n:END:\n"
         ;; "* %(eval org-xob--last-title) %((progn (org-entry-put (point) "CREATED" \"when\") \"\"))   :PROPERTIES:\n:TYPE:\t\t\tn.n\n:CREATED:\t\t%U\n:MODIFIED:\t\t%U\n\n:END:\n:BACKLINKS:\n:END:\n"

         :exobrain-node t
         :ntype "node"
         :func (lambda () t)
         :immediate-finish t
         :empty-lines-after 1)

        ("ad" "today" entry (function (lambda () (find-file (concat org-xob-path org-xob--log-file))))
         "* Day Log %u\n:PROPERTIES:\n:TYPE:\t\t\ta.day\n:END:\n:BACKLINKS:\n:END:\n"
         :exobrain-node t
         :func (lambda () t)
         ;; :func (lambda () (progn
         ;;                     (org-insert-subheading '(4))
         ;;                     (insert "hello world")))
         :immediate-finish t
         :ntype "a.day")

        ("ap" "new project" entry (file org-xob--agenda-file)
         "* Day Log %u\n:PROPERTIES:\n:TYPE:\t\t\tactivity.day\n:END:\n:BACKLINKS:\n:END:\n"
         :exobrain-node t
         )

        ("as" "new session" entry (file org-xob--agenda-file))

        ("tf" "todo general" entry (file "KB-file-000.org")
         "* %^{description} \n:BACKLINKS:\n:END:\n\n%?"
         :exobrain-node t
         :todo t
         :ntype "a.todo"
         )

        ("tp" "todo project" entry (file "KB-file-000.org")
         "* %^{description} \n:BACKLINKS:\n:END:\n\n%a\n%?"
         :exobrain-node t
         :todo t
         :ntype "a.todo"
         )
        ))

(org-xob--capture "hallo world")

(setq xob-logfile (concat org-xob-path org-xob--log-file))

(and t (if (not org-xob-today) 
           (setq org-xob-today (org-xob--capture "ad"))))

(setq org-xob-today nil)

(org-capture-expand-file (concat org-xob-path org-xob--log-file))
(org-capture-set-target-location (with-current-buffer ))

(setq org-xob-workspace "/Users/Will/exobrain/" )
(setq org-xob-path "/Users/Will/exobrain/xob/xob-logfile.org" )

(find-file "/Users/Will/exobrain/xob/xob-logfile.org" )
(node-title (gethash "88934E1F-AEE0-4672-A5BA-ECFD5A32EFC7" org-xob--id-node))
(node-title (gethash "3E5F74F6-D0BB-4BA4-8B73-9280AEE81955" org-xob--id-node))
(gethash "3E5F74F6-D0BB-4BA4-8B73-9280AEE81955" org-xob--id-node)

(require 'org-super-links)

(defun org-subtree-to-indirect-buffer ()
  (interactive)
  (let ((org-xob-short-title (concat (buffer-name) "-narrowclone")))
    (if (get-buffer org-xob-short-title)
        (kill-buffer org-xob-short-title))
    (clone-indirect-buffer-other-window org-xob-short-title t)
    (org-narrow-to-subtree)
    (switch-to-buffer org-xob-short-title)))
(setq bb (find-file "KB-file-000.org"))

;;; auto kill context buffer
(add-hook 'kill-buffer-hook #'org-xob--kill-context-buffer-hook nil :local)

(defun org-xob--kill-context-buffer-hook ()
  "Kill the context buffer when closing the node edit buffer. Made local variable.")

;;; attempt to append kb files for org-id
(listp org-agenda-text-search-extra-files)

(symbol-plist org-id-extra-files)

(setq org-id-extra-files (cl-set-difference org-id-extra-files org-xob--KB-files))
;; (mapcar (lambda (x) (setq ac-sources (delq x ac-sources)))
;;         '(ac-source-dictionary ac-source-words-in-same-mode-buffers))
