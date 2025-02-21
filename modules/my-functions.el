;;; functions.el --- Custom functions for modal editing -*- lexical-binding: t -*-

;;; Commentary:
;; Custom functions used by the modal editing system


;; Define a fallback function for unbound keys in delete mode
(defun my-modal-delete-mode-handle-unbound ()
  "Handle unbound keys in delete mode by switching to insert mode."
  (interactive)
  (let ((key (this-command-keys)))
    (my-modal-enter-insert-mode)
    (push key unread-command-events)))


(defun my-delete-char-left ()
  "Delete character to the left."
  (interactive)
  (progn
  (delete-char -1)
  (my-modal-enter-insert-mode)))

(defun my-delete-char-right ()
  "Delete character to the right."
  (interactive)
  (progn
  (delete-char 1)
  (my-modal-enter-insert-mode)))

(defun my-delete-char-at-point ()
  "Delete the character at point."
  (interactive)
  (progn
  (delete-char 1)
  (my-modal-enter-insert-mode)))



(defun my-delete-line-end ()
  "Delete from cursor to end of line."
  (interactive)
  (progn
   (delete-region (point) (line-end-position))
   (my-modal-enter-insert-mode)))

(defun my-delete-line-start ()
  "Delete from cursor to start of line."
  (interactive)
  (progn
  (delete-region (line-beginning-position) (point))
  (my-modal-enter-insert-mode)))

(defun delete-buffer-content ()
  "Delete the entire content of the current buffer."
  (interactive)
  (progn
  (delete-region (point-min) (point-max))
  (my-modal-enter-normal-mode)))

(defun delete-word-at-cursor ()
  "Delete the word where the cursor is positioned, including any space after it."
  (interactive)
  (let ((word-bounds (bounds-of-thing-at-point 'word)))
    (when word-bounds
      (let ((word-end (cdr word-bounds)))
        (delete-region (car word-bounds)
                      (if (and (< word-end (line-end-position))
                             (eq (char-after word-end) ?\s))
                          (1+ word-end)
                        word-end)))
                        (my-modal-enter-insert-mode))))

(defun my-delete-word-backward-safe ()
  "Safely delete the word before point."
  (interactive)
  (if (or buffer-read-only (bobp)) ;; Check if buffer is read-only or at the beginning
      (message "Cannot delete: buffer is read-only or at beginning of buffer")
    (let ((start (point)))
      (backward-word)
      (delete-region (point) start)
      (my-modal-enter-insert-mode))))

(defun my-delete-word-forward-safe ()
  "Safely delete the word after point."
  (interactive)
  (if (or buffer-read-only (eobp)) ;; Check if buffer is read-only or at the end
      (message "Cannot delete: buffer is read-only or at end of buffer")
    (let ((start (point)))
      (forward-word)
      (delete-region start (point))
      (my-modal-enter-insert-mode))))




;;======== deletions through visual mode


;; keybiding "d" in visual mode

 (defun my-visual-delete ()
  "Delete selected region, save to kill ring, and return to insert mode."
  (interactive)
  (when (region-active-p)
    (kill-region (region-beginning) (region-end)))
  (deactivate-mark))

;; keybiding "di" in visual mode
(defun find-delimited-bounds ()
  "Find bounds of delimited content under cursor."
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (pos (point))
         start end)
    (save-excursion
      ;; Find start delimiter
      (while (and (> (point) line-start)
                  (not start))
        (backward-char)
        (let ((char (string (char-after))))
          (when (or (string-match-p "[\"'`({[<*]" char)
                    (and (string= char "*")
                         (not (string= (string (char-before)) "*"))))
            (setq start (point)))))
      ;; Find end delimiter
      (when start
        (goto-char (1+ start))
        (let ((open-char (string (char-after start))))
          (while (and (< (point) line-end)
                     (not end))
            (forward-char)
            (let ((curr-char (string (char-before))))
              (when (or (and (string= open-char "\"") (string= curr-char "\""))
                       (and (string= open-char "'") (string= curr-char "'"))
                       (and (string= open-char "`") (string= curr-char "`"))
                       (and (string= open-char "(") (string= curr-char ")"))
                       (and (string= open-char "{") (string= curr-char "}"))
                       (and (string= open-char "[") (string= curr-char "]"))
                       (and (string= open-char "<") (string= curr-char ">"))
                       (and (string= open-char "*") (string= curr-char "*")))
                (setq end (point))))))))
    (when (and start end
               (<= start pos end))
      (cons start end))))


(defun delete-between-equal-chars ()
  "Delete content between matching characters, preserving the delimiters."
  (interactive)
  (let ((bounds (find-delimited-bounds)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (forward-char 1)
        (let ((start (point))
              (end (1- (cdr bounds))))
          (kill-region start end)))
      (my-modal-enter-insert-mode))))

(defun delete-including-equal-chars ()
  "Delete content between matching characters including the delimiters."
  (interactive)
  (let ((bounds (find-delimited-bounds)))
    (when bounds
      (kill-region (car bounds) (cdr bounds))
      (my-modal-enter-insert-mode))))

(defun my-delete-around-paragraph ()
  "Delete paragraph and surrounding blank lines."
  (interactive)
  (let ((start (save-excursion
                 (backward-paragraph)
                 (skip-syntax-backward " >")
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (skip-syntax-forward " >")
               (point))))
    (kill-region start end)))



;;========================= Visual Mode ========================================

(defun my-visual-select-word ()
  "Select the word at point if in visual mode."
  (interactive)
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (when bounds
        (goto-char (car bounds))
        (set-mark (point))
        (goto-char (cdr bounds)))))

(defun my-visual-select-line ()
  "Select the current line if in visual mode."
  (interactive)
    (beginning-of-line)
    (set-mark (point))
    (end-of-line))

(defun my-visual-select-paragraph ()
  "Select the paragraph at point."
  (interactive)
    (let ((start (save-excursion
                   (backward-paragraph)
                   (skip-chars-forward "\n\t ")
                   (point)))
          (end (save-excursion
                 (forward-paragraph)
                 (skip-chars-backward "\n\t ")
                 (point))))
      (goto-char start)
      (set-mark (point))
      (goto-char end)))

(defun bounds-of-text-between-spaces ()
  "Find the bounds of text between spaces around point."
  (interactive)
  (save-excursion
    (let (start end)
      ;; Find start - move backward to space or buffer start
      (if (re-search-backward "[[:space:]\n]" nil t)
          (setq start (1+ (point)))
        (setq start (point-min)))

      ;; Find end - move forward to space or buffer end
      (goto-char start)
      (if (re-search-forward "[[:space:]\n]" nil t)
          (setq end (1- (point)))
        (setq end (point-max)))

      ;; Return bounds if we found valid text
      (when (< start end)
        (cons start end)))))

(defun my-visual-select-between-spaces ()
  "Select text between spaces around point when in visual mode."
  (interactive)
  (let ((bounds (bounds-of-text-between-spaces)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (point))
      (goto-char (cdr bounds)))))


(defun visual-between-equal-chars ()
  "Select content between matching characters."
  (interactive)
  (let* ((bounds (find-delimited-bounds)))  ; Using our helper function
    (when bounds
      (goto-char (car bounds))
      (forward-char 1)
      (set-mark (point))
      (goto-char (1- (cdr bounds))))))


(defun visual-including-equal-chars ()
  "Select content including matching characters."
  (interactive)
  (let* ((bounds (find-delimited-bounds)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (point))
      (goto-char (cdr bounds)))))



;; ============= Yank mode ====================

(defun visual-yank ()
  "Copy the selected region to Emacs' kill ring."
  (interactive)
  (when (use-region-p)
    (kill-ring-save (region-beginning) (region-end))
    (deactivate-mark)
    (message "Copied: %s" (current-kill 0 t))
    (my-modal-enter-normal-mode)))

(defun visual-paste ()
  "Paste from Emacs' kill ring."
  (interactive)
  (when (car kill-ring)
    (yank)
    (my-modal-enter-normal-mode)))

(defun visual-paste-from-history ()
  "Show kill ring history in minibuffer and paste selected entry."
  (interactive)
  (when kill-ring
    (let ((selected (completing-read
                    "Select text to paste: "
                    kill-ring
                    nil t)))
      (when selected
        (insert selected)))))




;; =========== Undo logic ==============


;; Enhanced undo/redo functions with interactive selection
(defvar undo-tree-history-directory-alist nil)  ; Forward declaration to avoid warnings

(defun my-get-buffer-changes ()
  "Get a list of buffer changes from undo-tree if available."
  (if (and (fboundp 'undo-tree-mode)
           (bound-and-true-p undo-tree-mode))
      ;; Get changes from undo-tree
      (let ((node (undo-tree-node-previous (undo-tree-current buffer-undo-tree)))
            changes)
        (while node
          (let ((change (nth 0 (undo-tree-node-undo node))))
            (when change
              (push (format "[%s] %s"
                          (format-time-string "%H:%M:%S" (current-time))
                          (cond
                           ((stringp change) change)
                           ((and (consp change) (stringp (car change)))
                            (car change))
                           (t "Buffer modification")))
                    changes)))
          (setq node (undo-tree-node-previous node)))
        changes)
    ;; Fallback for standard undo
    (let ((changes nil)
          (undo-list buffer-undo-list))
      (while undo-list
        (let ((change (car undo-list)))
          (when (and (consp change) (stringp (car change)))
            (push (format "%s" (car change)) changes)))
        (setq undo-list (cdr undo-list)))
      changes)))

(defun my-enhanced-redo ()
  "Enhanced redo function with interactive change selection."
  (interactive)
  (let* ((changes (my-get-buffer-changes))
         (selected (if changes
                      (completing-read "Select change to redo: "
                                     changes
                                     nil t nil 'buffer-undo-history)
                    (user-error "No changes to redo"))))
    (if (and (fboundp 'undo-tree-mode)
             (bound-and-true-p undo-tree-mode))
        ;; Use undo-tree
        (let ((count 0)
              (found nil))
          (catch 'found
            (dolist (change changes)
              (when (string= change selected)
                (setq found t)
                (throw 'found t))
              (setq count (1+ count))))
          (when found
            (dotimes (_ count)
              (undo-tree-redo 1))
            (message "Redone to: %s" selected)))
      ;; Fallback implementation
      (let ((count 0)
            (found nil))
        (catch 'found
          (dolist (change changes)
            (when (string= change selected)
              (setq found t)
              (throw 'found t))
            (setq count (1+ count))))
        (when found
          (let ((modified (buffer-modified-p))
                (undo-in-progress t))
            (primitive-undo count buffer-undo-list)
            (setq buffer-undo-list pending-undo-list))
          (message "Redone to: %s" selected))))))

(defun my-enhanced-undo ()
  "Enhanced undo function with undo-tree support and fallback to regular undo."
  (interactive)
  (if (and (fboundp 'undo-tree-mode)
           (bound-and-true-p undo-tree-mode))
      (progn
        (undo-tree-undo 1)
        (message "Performed undo using undo-tree"))
    (progn
      (undo-only)  ; Using undo-only instead of undo
      (message "Performed standard undo"))))

;; Optional: Enable undo-tree-mode globally if available
(when (require 'undo-tree nil 'noerror)
  ;; Configure undo-tree before enabling it
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  (global-undo-tree-mode 1))

;; Optional: Customize undo behavior
(setq undo-limit 80000000)           ; Raise undo-limit to 80Mb
(setq undo-strong-limit 120000000)   ; Raise undo-strong-limit to 120Mb
(setq undo-outer-limit 160000000)    ; Raise undo-outer-limit to 160Mb


;; Insert of delimiters in visual mode  =====================================================


;; Define delimiter pairs
(defvar my-delimiter-pairs
  '((?1 . ("(" . ")"))    ; Number 1 for parentheses
    (?2 . ("[" . "]"))    ; Number 2 for square brackets
    (?3 . ("{" . "}"))    ; Number 3 for curly braces
    (?4 . ("\"" . "\""))  ; Number 4 for double quotes
    (?5 . ("'" . "'"))    ; Number 5 for single quotes
    (?6 . ("`" . "`"))    ; Number 6 for backticks
    (?7 . ("<" . ">"))    ; Number 7 for angle brackets
    (?8 . ("/*" . "*/"))  ; Number 8 for C-style comments
    (?9 . ("<!--" . "-->")) ; Number 9 for HTML comments
    (?0 . ("_" . "_")))   ; Number 0 for underscores
  "Alist of delimiter pairs keyed by number keys.")

(defun my-surround-region-with-delimiter (start end delimiter-key)
  "Surround the region between START and END with delimiters specified by DELIMITER-KEY."
  (interactive "r\nc")  ; Read region and a character
  (let* ((delimiter-pair (cdr (assoc delimiter-key my-delimiter-pairs)))
         (opening (car delimiter-pair))
         (closing (cdr delimiter-pair)))
    (when delimiter-pair
      (save-excursion
        (goto-char end)
        (insert closing)
        (goto-char start)
        (insert opening)))))

(defun my-visual-surround-with-delimiter (delimiter-key)
  "Surround the current visual selection with delimiters."
  (interactive "c")
  (when (region-active-p)
    (my-surround-region-with-delimiter (region-beginning) (region-end) delimiter-key)
    (my-modal-enter-normal-mode)))  ; Exit visual mode after surrounding

;; Define commands for each number key
(defun my-visual-surround-1 () (interactive) (my-visual-surround-with-delimiter ?1))
(defun my-visual-surround-2 () (interactive) (my-visual-surround-with-delimiter ?2))
(defun my-visual-surround-3 () (interactive) (my-visual-surround-with-delimiter ?3))
(defun my-visual-surround-4 () (interactive) (my-visual-surround-with-delimiter ?4))
(defun my-visual-surround-5 () (interactive) (my-visual-surround-with-delimiter ?5))
(defun my-visual-surround-6 () (interactive) (my-visual-surround-with-delimiter ?6))
(defun my-visual-surround-7 () (interactive) (my-visual-surround-with-delimiter ?7))
(defun my-visual-surround-8 () (interactive) (my-visual-surround-with-delimiter ?8))
(defun my-visual-surround-9 () (interactive) (my-visual-surround-with-delimiter ?9))
(defun my-visual-surround-0 () (interactive) (my-visual-surround-with-delimiter ?0))

;; Optional: Add custom delimiter pairs
;; (my-add-delimiter-pair ?+ "+" "+")  ; For +emphasizing+ text
(defun my-add-delimiter-pair (key opening closing)
  "Add a new delimiter pair to my-delimiter-pairs."
  (interactive)
  (add-to-list 'my-delimiter-pairs (cons key (cons opening closing))))


;;=====================================================end of functions =========================
(provide 'my-functions)
;;; my-modal-funcs.el ends here
