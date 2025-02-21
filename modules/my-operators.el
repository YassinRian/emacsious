(provide 'my-operators)

(defun my-delete-region (beg end)
  "Delete text from BEG to END."
  (delete-region beg end))

(defun my-yank-region (beg end)
  "Yank (copy) text from BEG to END."
  (kill-ring-save beg end))
