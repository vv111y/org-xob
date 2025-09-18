;; Method 1: Direct load (best for testing)
(load-file "/Users/will/DevWorkspace/1MyTools/Emacs/zettle/org-xob/org-xob.el")

;; Method 2: Using require (if directory is in load-path)
(add-to-list 'load-path "/Users/will/DevWorkspace/1MyTools/Emacs/zettle/org-xob/org-xob.el")
(require 'org-xob)
