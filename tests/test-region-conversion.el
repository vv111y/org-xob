;;; test-region-conversion.el --- Test region to node conversion feature

;;; Commentary:
;; Simple validation test for the region to node conversion functionality

;;; Code:

;; Test 1: Verify backend functions exist
(unless (fboundp 'org-xob--region-to-node-with-link)
  (error "Backend function org-xob--region-to-node-with-link not found"))

(unless (fboundp 'org-xob--node-to-region)
  (error "Backend function org-xob--node-to-region not found"))

;; Test 2: Verify interactive functions exist
(unless (fboundp 'org-xob-region-to-node)
  (error "Interactive function org-xob-region-to-node not found"))

(unless (fboundp 'org-xob-node-to-region)
  (error "Interactive function org-xob-node-to-region not found"))

;; Test 3: Verify functions are autoloaded
(unless (get 'org-xob-region-to-node 'function-documentation)
  (message "Warning: org-xob-region-to-node may not be properly autoloaded"))

(unless (get 'org-xob-node-to-region 'function-documentation)
  (message "Warning: org-xob-node-to-region may not be properly autoloaded"))

(message "✓ All region-to-node conversion functions are properly defined")

;; Test 4: Manual testing instructions
(message "Manual testing instructions:")
(message "1. Select some text in an org-xob buffer")
(message "2. Run M-x org-xob-region-to-node or press 'r' in hydra")
(message "3. Enter a title for the new node")
(message "4. The text should be replaced with a link to the new node")
(message "5. Click on the link and press 'R' in hydra or run M-x org-xob-node-to-region")
(message "6. The link should be converted back to the original text")

;;; test-region-conversion.el ends here
