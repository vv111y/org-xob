;;; test-bulk-operations.el --- Test bulk context operations feature

;; Simple test for the bulk context operations functionality

;; Load the xob system
(require 'org-xob-core)
(require 'org-xob-backend)
(require 'org-xob)

;; Test function to validate bulk operations functionality
(defun test-org-xob-bulk-operations ()
  "Test the bulk context operations functionality."
  (interactive)
  
  ;; Test bulk operation functions exist
  (message "Testing bulk operation functions...")
  
  (assert (fboundp 'org-xob--map-all-sources)
          t "org-xob--map-all-sources function should be defined")
  
  (assert (fboundp 'org-xob-bulk-to-summary)
          t "org-xob-bulk-to-summary function should be defined")
  
  (assert (fboundp 'org-xob-bulk-to-section)
          t "org-xob-bulk-to-section function should be defined")
  
  (assert (fboundp 'org-xob-bulk-to-node-tree)
          t "org-xob-bulk-to-node-tree function should be defined")
  
  (assert (fboundp 'org-xob-bulk-to-full-node)
          t "org-xob-bulk-to-full-node function should be defined")
  
  (assert (fboundp 'org-xob-bulk-clear-all)
          t "org-xob-bulk-clear-all function should be defined")
  
  ;; Test that hydra is properly extended
  (message "Testing hydra extension...")
  (assert (boundp 'org-xob-hydra/keymap)
          t "org-xob-hydra should be defined")
  
  (message "All bulk operations tests passed!"))

;; Run test when file is loaded
(test-org-xob-bulk-operations)

;;; test-bulk-operations.el ends here
