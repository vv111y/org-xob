;;; workfile.el --- a summary
;;; axiv
;;;; yes/no - use org-capture instead

(if (not sel in all-nodes)
    (if (y-or-n-p-with-timeout "create new node? " 6 nil)
        (org-exobrain-add-node sel)))
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
;; (defun org-exobrain--table-load ()
;;   "Load the node lookup hash table."
;;   (setq org-exobrain--title-id nil)
;;   (with-temp-buffer
;;     (condition-case nil
;;         (progn
;;           (insert-file-contents org-id-locations-file)
;;           (setq org-id-locations (read (current-buffer)))
;;           (let ((loc (file-name-directory org-id-locations-file)))
;;             (mapc (lambda (item)
;;                     (unless (file-name-absolute-p (car item))
;;                       (setf (car item) (expand-file-name (car item) loc))))
;;                   org-id-locations)))
;;       (error
;;        (message "Could not read org-id-values from %s.  Setting it to nil."
;;                 org-id-locations-file))))
;;   (setq org-id-files (mapcar 'car org-id-locations))
;;   (setq org-id-locations (org-id-alist-to-hash org-id-locations)))
;;; binding trials
(defun vv/afn ()
  (message "I see `c', its value is: %s" c))

(defvar c t)

(let ((a "I'm lexically bound")
      (c "I'm special and therefore dynamically bound"))
  (message "I see `a', its values is: %s" a))

;;; parse ast fns

(measure-time (org-element-parse-buffer))
(measure-time (setq vv-ast (org-element-parse-buffer)))

(setq vv-ast nil)

(defun vv-get-ast ()
  (interactive)
  (setq vv-ast (org-element-parse-buffer)))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "parse time: %.06f" (float-time (time-since time)))))


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
;;; make server
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

(org-element-interpret-data '(("ID" . "idee") ("ITEM" . "heading")))



(org-element-interpret-data '(headline (:raw-value "search: internet global" :begin 11544 :end 11577 :pre-blank 0 :contents-begin nil :contents-end nil :level 4 :priority nil :tags nil :todo-keyword #("[.]" 0 3 (fontified t line-prefix #("***" 0 3 (face org-indent)) wrap-prefix #("******* " 0 3 (face org-indent) 3 8 (face org-indent)) face (:height 70) org-category "org-exobrain")) :todo-type todo :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 11544 :title "search: internet global")))



#("**** [.] search: internet global
" 5 8 (org-category "org-exobrain" face (:height 70) wrap-prefix #("******* " 0 3 (face org-indent) 3 8 (face org-indent)) line-prefix #("***" 0 3 (face org-indent)) fontified t))


(org)

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

;;; pulse highlight indicator
(pulse-momentary-highlight-one-line (point))
(pulse-lighten-highlight)
;;; linking
(setq org-id-link-to-org-use-id t)

(progn (setq vv-b (org-id-store-link)) (message vv-b))

(org-element-property :ID (org-element-at-point))
(org-element-property :CPARENTS (org-element-at-point))

;;; misc 
(require 'org-ql)
(org-ql-search 'all
  '(property "CPARENTS"))

;;; go round: get node text?

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
        (when (org-element-property :raw-value "Accessors" el)
          (org-element-contents el)
          (message el))))
    )
   ))


;; [[https://emacs.stackexchange.com/questions/48430/get-section-text-with-elisp][org mode - Get Section text with elisp - Emacs Stack Exchange]]  [2020-07-19 Sun 07:24]

(require 'subr-x) ;; for when-let

(defun vv-get-headline-with-text ()
  ;; "Return a list with the headline text of the top-level headline for point as first element and the section text as second element."
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
        ;; (list headline-text (buffer-substring text-begin text-end))
        (list headline-text (buffer-substring text-begin text-end))
        ))))

(ediff-save-buffer '4)
;; average english word length = 4.7
;; 524,288 / 4.7 = 111,550 words / KB file
;; if average node = 250 words -> 111,550/250 = 446 nodes per file
;;; shell calls
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
    ,(org-exobrain--diff-filename 'node)
    )
  ))

(defun org-exobrain--diff-node (node)
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
      ,(org-exobrain--diff-filename 'node)
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

(remove-if (lambda (buf) (with-current-buffer buf (not org-exobrain))) (buffer-list))
(remove-if (lambda (buf) (with-current-buffer buf org-exobrain)) (buffer-list))

(remove-if  (with-current-buffer buf org-exobrain) (buffer-list))

(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when org-exobrain
      (when (eq major-mode 'org-mode)
        (print buf)))
    ))

(org-exobrain--active-buffers)

(remove-if-not (lambda (buf) (buffer-modified-p buf)) (org-exobrain--active-buffers))
(remove-if-not #'buffer-modified-p (org-exobrain--active-buffers))

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

;;; org-id playing

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

;;; helm
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
                               ;; (let* ((cans (hash-table-keys org-exobrain--title-id))
                                      )
                                 (cons helm-input cans)))
                 ;; :filtered-candidate-transformer #'(lambda (cans source)
                 ;;                                     (cons "[?] " cans))
                 :volatile t
                 ;; :action (list (cons "new node" (lambda (key) (org-exobrain--capture key))))
                 :action (lambda (key) (let ((ID (gethash key org-id-locations)))
                 ;; :action (lambda (key) (let ((ID (gethash key org-exobrain--title-id)))
                                         (if ID
                                             (org-exobrain--get-entry ID)
                                           (org-exobrain--capture key)))))
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
                                  ;;     (org-exobrain--get-entry id))
                                  (org-exobrain--capture key))))))


(helm :sources
      `((name . "Exobrain Nodes")
        (candidates . ,(hash-table-keys org-exobrain--title-id))
        (action . (lambda (key) (let ((ID (gethash key ,org-exobrain--title-id)))
                                  (if ,ID
                                      (org-exobrain--get-entry ,ID)
                                    (org-exobrain--capture helm-input)))))))

;;; hash table 
;; create empty table
(setq org-exobrain--table-size 10000)
(setq org-exobrain--title-id (make-hash-table
                              :test 'equal
                              :size org-exobrain--table-size))

;; store node title and id NewNode
(puthash title id org-exobrain--title-id)

;; get the node location from title
(gethash (gethash title org-exobrain--title-id) org-id-locations)

;;;; rough
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

;;; load/save hashtable
;; https://stackoverflow.com/questions/11745097/serializing-an-emacs-lisp-hash-to-file
(featurep 'hashtable-print-readable)  ;; check if this feature is true, then can use prin1


;; https://stackoverflow.com/questions/36193866/idiomatic-way-serialization-in-emacs-lisp
;; write
(defun org-exobrain--save-object (file data)
  (with-temp-file file
    (prin1 data (current-buffer))))

;; Read from file:
(defun org-exobrain--load-object (file symbol)
  (when (boundp symbol)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (set symbol (read (current-buffer))))))

;; Call to write:
(my-write "test.txt" org-id-locations)

;; Call to read
(my-read "test.txt" 'my-emacs-version)
;;; org-capture
;;;; other capture hooks

;; (remove-hook 'org-capture-mode-hook #'org-exobrain--capture-create-id)
org-capture-mode-hook ;; after entering or leaving
org-capture-prepare-finalize-hook ;; capture buffer narrowed
org-capture-before-finalize-hook  ;; capture buffer widened
org-capture-after-finalize-hook ;; done. for closing stuff

;;;; old capture fn for hook
;; (defun org-exobrain--new-node (title)
;; (defun org-exobrain--new-node ()
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

(defun org-exobrain--capture (title)
  (let* ((org-capture-templates org-exobrain--templates)
         ID)
    ;; TODO test
    (if (member 'title 'org-exobrain--auto-types)
        (org-capture nil title)
      (org-capture))
    ID))

(defun org-exobrain--new-node ()
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
      (setq node (make-node :title title
                            :type type 
                            :backlinks (list)))
      (puthash title ID org-exobrain--title-id)
      (puthash ID node org-exobrain--id-node)
      ID)))

(add-hook 'org-capture-mode-hook #'org-exobrain--new-node)

(defvar org-exobrain--auto-types '(("day" . a.day)
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

(defvar org-exobrain--templates
      '(("f" "fast" entry (file org-exobrain--KB-file)
         "* %(eval title)  :node:\n%?\n** backlinks :bl:"
         :exobrain-node t
         :ntype "node"
         :vid "0"
         ;; :immediate-finish t
         :empty-lines-after 1)
        ("ct" "today" entry (file org-exobrain--KB-file)
         "* %() :node:\n\n** backlinks :bl:"
         :exobrain-node t
         :immediate-finish t
         :ntype "context.day")))

;;;;; capture scraps

;; (org-capture-string "f")
(org-capture nil "lw")
;; (plist-get 'org-capture-plist :key)
;; (defun org-exobrain--link-nodes (ida idb)
;;   (org-id-open ida))

;; (defun org-exobrain--get-entry (key)
;;   (format "getting entry %s" key))

;;;; create buffer
(copy-to-buffer "testbuff" (point-min) (point-max))
(defun org-exobrain-open-node-buffer (item)
  (interactive)
  (with-current-buffer (get-buffer-create item)
    (unless org-exobrain-mode) (org-exobrain-mode)
    ;; insert node contents
    )
  )

;;; main get node  
;;;; testing version for reference
;; (setq org-exobrain-today nil)

;; (defun org-exobrain-get-node ()
;;   (interactive)
;;   (when (not org-exobrain-on-p)
;;     (org-exobrain-start))
;;   (unless org-exobrain-today
;;     (setq org-exobrain-today (org-exobrain--capture "today")))
;;   ;; Get node by title, or create new one 
;;   (helm :buffer "*xob get node*"
;;         :sources (helm-build-sync-source "vv-sss"
;;                    :candidates (lambda ()
;;                                  (let* ((candidates (hash-table-keys org-id-locations))
;;                                         ;; (let* ((cans (hash-table-keys org-exobrain--title-id))
;;                                         )
;;                                    (cons helm-input candidates)))
;;                    :volatile t
;;                    ;; :action (lambda (title) (let ((ID (gethash title org-exobrain--title-id)))
;;                    :action (lambda (title) (let ((ID (gethash title org-id-locations)))
;;                                            (unless ID
;;                                              (setq ID (org-exobrain--capture title)))
;;                                            (org-exobrain--activate-node ID))))))

;;;; alternative to get contents, used in activate-node fn
;; (contents (save-mark-and-excursion (progn 
;;                                      (marker-buffer m)
;;                                      (goto-char m)
;;                                      (move-marker m nil)
;;                                      (org-copy-subtree)))))

;;;; misc
;; (cl-pushnew org-exobrain-today (node-backlinks
;;                                 (org-exobrain--id-node ID)))

;; old idea
;; (defun org-exobrain--find-node ()
;;   ;;
;;   ;; fuzzy search node titles
;;   (let ((pos (org-id-find id)))
;;     ;; get node (header+contents) at pos (filename . possible))
;;     ))
;;; possible state vars for active workspace. so far don't need this way:
;; (defvar org-exobrain--active-list nil
;;   "Alist of active nodes and their buffer locations.")
;; OR check for ID in buffers
;; (defun org-exobrain--active-p (ID)
;;   "Returns t if the node is already in the workspace."
;;   nil)
;; (defun org-exobrain--get-buffer (ID)
;;   nil
;;   )
;; (setq ID )


;;; node structs and hashtable
;; (cl-defstruct node title type backlinks)
;; (setq vv/ns (make-node :title "meee" :backlinks (list)))
;; nice to know, not using it 
;; (setf (node-backlinks vv/ns) (append '(a)))
;;; trial #2 state : alternate use struct for state
(cl-defstruct xob-state kb-count kb-current kb-files t-id-table-fn id-n-table-fn)
(setq xob (make-xob-state :kb-count 0 :t-id-table-fn "title-id-table" :id-n-table-fn "id-node-table"))

(defun xob-new-file ()
  (interactive)
  (let ((filename (concat 
                          org-exobrain--KB-filename-prefix
                          (format "%03d" (xob-state-kb-count xob))
                          ".org")))
    (with-temp-buffer
      (write-file filename))
    (cl-pushnew filename (xob-state-kb-files xob))
    (setf (xob-state-kb-current xob) filename)
    (setf (xob-state-kb-count xob) (+ 1))))

(xob-state-id-n-table-fn xob)

(setf (xob-state-kb-current xob) "KB-file-000.org")

(org-exobrain--save-object "xob_state" xob)
(prin1 xob)
(symbol-value xob)
(prin1 org-exobrain--title-id)

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
(defun org-exobrain--new-KB-file ()
  "Create new KB file for next node in the brain."
  (let ((filename (concat org-exobrain-path
                          org-exobrain--KB-filename-prefix
                          (format "%03d" (length (directory-files org-exobrain-path nil org-exobrain--KB-filename-prefix)))
                          ".org")))
    (with-temp-buffer
      (write-file filename))
    (push filename org-exobrain--KB-files)))
;;; looping 
(type-of (cl-position 2 '(6 5 4 2 1)))

(-repeat 1000 '2)


(cl-dolist (el org-exobrain--objects)
  (format "obj: %s   name: %s" (car el) (cdr el)))

(loop for (k . v) in org-exobrain--objects
      do (format "%s || %s" k v))

(cl-loop for key in (mapcar 'car org-exobrain--objects)
         for value in (mapcar 'cdr org-exobrain--objects)
         ;; collect (cons value key)
         collect (format "%s || %s" key value)
         ;; collect key value
         )

(format "O: %s ||| N: %s" (car (car org-exobrain--objects)) (cdr (car org-exobrain--objects)))

;; this works
(cl-loop for (k . v) in org-exobrain--objects
         collect (format "%s || %s" k v)
         )
;;; var prob
(org-exobrain--activate-node '"5fc3aafe-fa83-4ec4-9db3-12e703d31bb2")
(org-exobrain--save-state)
(symbol-value (car (car org-exobrain--objects)))
(setq org-exobrain--title-id 4)

(setq org-exobrain--objects '((org-exobrain--title-id . "title-id-table")
                              (org-exobrain--id-node . "id-node-table")
                              (org-exobrain--KB-file . "current-KB-file")))


(cl-loop for (k . v) in org-exobrain--objects
         do (progn
              ;; (print k)
              (print (symbol-value k))
              (set k 4)
              (print v)
              ;; (setq (symbol-value k) 4)
              ))

;;; org heading stuff

(setq vv-p nil)
(length vv-p)
(org-map-entries (lambda () (push (nth 4 (org-heading-components)) vv-p)))
(org-map-entries (lambda () (push (cons (point) (nth 4 (org-heading-components))) vv-p)))
(org-map-entries (lambda () (push (point) vv-p)))

;;;; ap's searches
(-flatten
(-non-nil
 (mapcar (lambda (file)
           (let ((case-fold-search nil))
             (with-current-buffer (find-buffer-visiting file)
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
             (with-current-buffer (find-buffer-visiting file)
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

;;; dual buffers/windows
;;;; atomic window ex.
 (let ((window (split-window-right)))
   (window-make-atom (window-parent window))
   (display-buffer-in-atom-window
    (get-buffer-create "*Messages*")
    `((window . ,(window-parent window)) (window-height . 5))))

;;;; sideline
(defun org-exobrain-open-sideline ()
  "Open context content in a side window."
  (interactive)
  ;; (org-exobrain-new-buffer)
  (let ((window (split-window-right)))
    ;; (window-make-atom (window-parent window))
    (display-buffer-in-atom-window
     (get-buffer-create "*node context*")
     `((window . ,(window-parent window)) (window-height . 5)))))

(defun org-exobrain-open-sideline ()
  "Open context content in a side window."
  (interactive)
  ;; (org-exobrain-new-buffer)
  (let ((window 
         (display-buffer-in-atom-window
          (get-buffer-create "*node context*")
          `((window . ,(selected-window)) (side . right)))))))

(window-atom-root)
(window-tree)

;;;
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

;;; views stage 1
