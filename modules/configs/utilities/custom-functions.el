;;; -*- lexical-binding: t -*-

;; Couple of handy function

(require 'hydra)

;; widen window based on text length
;; =======================================================
(defun fit-window-to-buffer-width (&optional window max-width min-width)
  "Fit Window according to its buffer's width"
  (interactive)
  (let ((fit-window-to-buffer-horizontally 'only))
    (fit-window-to-buffer window nil nil max-width min-width)))

;;--------------------------------------------------------
;; split window right (will be used for dired)
;; =======================================================
(defun dired-in-vertical-split ()
  (interactive)
  (split-window-right)
  (other-window 1)
  )

;;--------------------------------------------------------
;; split window vertical
;; =======================================================
(defun buffer-in-vertical-split ()
  (interactive)
  (split-window-horizontally);; een beetje rare term, maar ze bedoelen dat 2 windows horizontaal worden gecreerd
  (other-window 1))


;;--------------------------------------------------------
;; kill all buffers except the current one
;; =======================================================

(defun only-current-buffer () 
  (interactive)                                                                   
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

;;--------------------------------------------------------
;; avy-run - this function will go to a line and a word can be searched in that line
;; =======================================================
(defun yr/avy-run (arg)
  (interactive "P")
  (avy-goto-line)
  (let ((avy-all-windows nil))
    (cl-letf (((symbol-function 'avy--find-visible-regions) (lambda (&rest args) `((,(point-at-bol) . ,(point-at-eol))))))
      (avy-goto-char-timer arg))))

(defun yr/avy-run-in-line (arg)
  (interactive "P")
  ;;(avy-goto-line)			       
  (let ((avy-all-windows nil))
    (cl-letf (((symbol-function 'avy--find-visible-regions) (lambda (&rest args) `((,(point-at-bol) . ,(point-at-eol))))))
      (avy-goto-char-timer arg))))



;;--------------------------------------------------------
;; delete next char
;; =======================================================
(defun delete-next-char ()
  "Delete the character after point."
  (interactive)
  (delete-char 1))

;;--------------------------------------------------------
;; indent text right (incrementally)
;; =======================================================
(defun my-indent-rigidly-right (arg)
  "Indent region rigidly to the right and keep region active."
  (interactive "p")
  (if (region-active-p)
      (progn
	(indent-rigidly (region-beginning) (region-end) arg)
	(setq deactivate-mark nil))
    (message "Region is not active")))

;;--------------------------------------------------------
;; indent text left (incrementally)
;; =======================================================
(defun my-indent-rigidly-left (arg)
  "Indent region rigidly to the right and keep region active."
  (interactive "p")
  (if (region-active-p)
      (progn
	(indent-rigidly (region-beginning) (region-end) (- arg))
	(setq deactivate-mark nil))
    (message "Region is not active")))

;;--------------------------------------------------------
;; widen window incrementally
;; =======================================================

(defun widen-window-incrementally ()
  "Widen the window by 10 columns."
  (interactive)
  (enlarge-window-horizontally 10)
  (enlarge-window 10))

;;--------------------------------------------------------
;; Delete other windows
;; =======================================================

(defun delete-other-windows-except-active ()
  "Delete all other windows except the active one."
  (interactive)
  (delete-other-windows (get-buffer-window (current-buffer) t)))

;;====================== Hooks ============================


;;--------------------------------------------------------
;; switch to Eshell and change to insert-mode
;; =======================================================
(defun my-switch-window-hook ()
  (interactive)
  "Function to run when switching to another window."
  (unless (equal (buffer-name) "*eshell*")
    (my-modal-enter-normal-mode)))

;;--------------------------------------------------------
;; switch Grep mode to command-state mode
;; =======================================================

;; Define a function to activate boon-set-command-state for *search-string* buffer
;; (defun my-check-search-string-buffer ()
;;   "Activate boon-set-command-state when switching to *search-string* buffer."
;;   (when (string-equal (buffer-name) "*search-string*")
;;     (boon-set-command-state)))

;; Add the function to window-configuration-change-hook
;;(add-hook 'window-configuration-change-hook 'my-check-search-string-buffer)

(defun my-check-special-buffers ()
  "Activate boon-set-command-state when switching to *search-string* or *Occur* buffers."
  (when (member (buffer-name) '("*search-string*" "*Occur*"))
    (boon-set-command-state)))

(add-hook 'buffer-list-update-hook 'my-check-special-buffers)

;;--------------------------------------------------------
;; toggle-window
;; =======================================================

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun my-split-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun my-split-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

;;================== Utility functions =============================


;;--------------------------------------------------------
;; a hook to switch to insert-mode when Eshell is opened.
;; =======================================================
;;(add-hook 'window-configuration-change-hook 'my-switch-window-hook)

;;--------------------------------------------------------
;; Interesting function call-interactively, my-switch-window-hook will run after ace-window
;; =======================================================
(advice-add 'ace-window :after '(lambda (&rest args)
                                  (call-interactively 'my-switch-window-hook)))

;;--------------------------------------------------------                                  
;; search files recursively !
;; =======================================================
(advice-add 'dired-recent-open :after '(lambda (&rest args)
					 (call-interactively 'find-file-in-current-directory)))

;;--------------------------------------------------------
;; open dired in vertical window
;; =======================================================
;;(advice-add 'dired-in-vertical-split :after '(lambda (&rest args)
;;                                             (call-interactively 'dired)))

;;--------------------------------------------------------
;; consult-buffer is run after a vertical buffer is opened
;; =======================================================
;;(advice-add 'buffer-in-vertical-split :after '(lambda (&rest args)
;;                                             (call-interactively 'consult-buffer)))
;;--------------------------------------------------------
;; consult-buffer is run after a vertical window is opened
;; =======================================================
(advice-add 'my-split-window-right :after '(lambda (&rest args)
                                             (call-interactively 'consult-buffer)))
;;--------------------------------------------------------
;; consult-buffer is run after a horizontal window is opened
;; =======================================================
(advice-add 'my-split-window-below :after '(lambda (&rest args)
                                             (call-interactively 'consult-buffer)))
;;--------------------------------------------------------
;; Other window call hydra so that the switching of windows stays active
;; ==================================================================

(defun my-other-window ()
  (interactive)
  (other-window))

(advice-add 'my-other-window :after '(lambda (&rest args)
                                    (call-interactively 'hydra-other-win/body)))

;;--------------------------------------------------------
;; after crux-smart-open-line hydra-move is opened
;; ==================================================================
(advice-add 'crux-smart-open-line :after '(lambda (&rest args)
					    (call-interactively 'hydra-move/body)))

(advice-add 'crux-smart-open-line-above :after '(lambda (&rest args)
						  (call-interactively 'hydra-move/body)))

;;--------------------------------------------------------
;; Boon-insert is run after Wdired is activated
;; =======================================================

(defun my-wdired ()
  (interactive)
  (wdired-change-to-wdired-mode))

(advice-add 'my-wdired :after '(lambda (&rest args)
                                 (call-interactively 'boon-insert)))

;;--------------------------------------------------------
;; hulpfunctie om te checken of wdired is actief
;; =======================================================

(defun my/check-dired-mode-active (func)
  (if (derived-mode-p 'wdired-mode)
      (funcall func)))


;;--------------------------------------------------------
;; hulpfunctie om in dired na Consult-line meteen in de folder
;; te gaan of het bestand te openen
;; =======================================================

(defun my-consult-line ()
  (interactive)
  (consult-line))

(advice-add 'my-consult-line :after '(lambda (&rest args)
                                       (call-interactively 'dired-find-file)))
;;--------------------------------------------------------
;; hulpfunctie om te checken of the entry in dired een bestand is of een directory
;; in geval van een bestand, dat wordt weergegeven in een nieuwe window
;; =======================================================

;; (defun my-dired-open ()
;;   "In dired, open the file or directory in different ways.
;; If it is a directory, use `dired-find-file`.
;; If it is a file, use `dired-find-file-other-window`."
;;   (interactive)
;;   (let ((file (dired-get-file-for-visit)))
;;     (if (file-directory-p file)
;;         (dired-find-file)
;;       (dired-find-file-other-window))))



(defun my-dired-find-file-other-window-vertically ()
  "Open the file at point in another window with a vertical split, reusing existing windows."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if-let ((win (get-window-with-predicate
                   (lambda (w)
                     (string= (buffer-file-name (window-buffer w)) file)))))
        (select-window win)
      (let ((target-window (if (one-window-p)
                               (split-window-right)
                             (next-window (selected-window) 1 'visible))))
        (select-window target-window)
        (find-file file)))))

(defun my-dired-open ()
  "In dired, open the file or directory in different ways.
If it is a directory, use `dired-find-file`.
If it is a file, use `my-dired-find-file-other-window-vertically`."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dired-find-file)
      (my-dired-find-file-other-window-vertically))))

;;--------------------------------------------------------
;; hulpfunctie om detail te verbergen in dired-mode
;; 
;; =======================================================

(defun my-dired-mode-setup ()
  "Custom settings for `dired-mode`."
  (dired-hide-details-mode 1))

(add-hook 'dired-mode-hook 'my-dired-mode-setup)

;;--------------------------------------------------------
;; Hulpfunctie om te zoeken naar een specifieke string in
;; alle bestanden van een bepaalde folder
;; 
;; =======================================================

(require 'compile)

(defun search-string-in-directory (search-string)
  "Search for SEARCH-STRING in all files within default-directory using find and awk."
  (interactive "sEnter search string: ")
  (let* ((directory (expand-file-name default-directory))
         (command (format "find %s -type f -exec awk '/%s/ {printf \"%%s:%%d:\\n\", FILENAME, FNR}' {} +"
                          (shell-quote-argument directory)
                          (shell-quote-argument search-string))))
    (compilation-start command 'grep-mode (lambda (mode-name) "*search-string*"))))

(add-to-list 'compilation-error-regexp-alist 'search-string)
(add-to-list 'compilation-error-regexp-alist-alist
             '(search-string "^\\(.+\\):\\([0-9]+\\):" 1 2))


(defcustom consult-dir-custom-command #'search-string-in-directory
  "Custom command to run after selecting a directory using `consult-dir'.

The default is to invoke `search-string-in-directory' from the chosen
directory. This can be customized to run any arbitrary function
(of no variables), which will be called with `default-directory'
set to the directory chosen using `consult-dir'."
  :group 'consult-dir
  :type '(function :tag "Custom function"))


;;--------------------------------------------------------
;;
;; 
;; =======================================================


(defun activate-next-word-region ()
  "Move to the next word and activate the region for that word."
  (interactive)
  (if (looking-at "\\w+")
      (goto-char (match-end 0)))
  (if (re-search-forward "\\w+" nil t)
      (progn
        (set-mark (match-end 0))
        (goto-char (match-beginning 0))
        (setq deactivate-mark nil))
    (message "No more words found")))

;;--------------------------------------------------------
;; corfu functie om te quiten wanneer hydra-change-mode is
;; activated
;; =======================================================

;; Function to quit Corfu
(defun my-corfu-quit ()
  "Quit Corfu completion."
  (when corfu--candidates
    (corfu-quit)))

;;--------------------------------------------------------
;; my-other-window is gebonden aan "ga" zodat the boon-insert-state wordt
;; toegepast op eshell
;; =======================================================

(defun my-other-window ()
  (interactive)
  (other-window 1))

(advice-add 'my-other-window :after '(lambda (&rest args)
                                       (call-interactively 'my-switch-window-hook)))


;;--------------------------------------------------------
;; custom function to duplicate a file in dired, added to keybindings (see dired hydra)
;; =======================================================
 
(defun dired-duplicate-this-file (suffix)
  "Duplicate file on this line."
  (interactive (list (read-string "Suffix: " "_COPY")))
  (dired-do-copy-regexp "\\(.*\\)\\.\\(.*\\)"
                        (concat "\\1" suffix ".\\2"))
  )
;;=============================================================================================================
;;--------------------------------------------------------
;; custom function to combine boon-smarter-forward & crux-smart-open-line
;; =======================================================

(defun my-boon-smarter-forward-advice (orig-fun &rest args)
  "Advice for `boon-smarter-forward` to run `crux-smart-open-line` when the end of the buffer is reached."
  (if (eobp)
      (boon-open-next-line-and-insert)
    (apply orig-fun args)))

(advice-add 'boon-smarter-forward :around #'my-boon-smarter-forward-advice)

;;=============================================================================================================
;;--------------------------------------------------------
;; custom function to duplicate current screen and split-window-horizontally
;; =======================================================

(defun duplicate-and-split-window-horizontally ()
  "Duplicate the current window and split it horizontally."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer current-buffer)))

;;=============================================================================================================
;;--------------------------------------------------------
;; custom function to create new file in current dir
;; =======================================================

(defun create-new-file-in-current-dir (filename)
  "Create a new file in the current directory and open it in a buffer.
Only prompts for the file name, and creates it in the current directory."
  (interactive "sEnter new file name: ")
  (let ((full-path (concat (file-name-as-directory default-directory) filename)))
    (if (file-exists-p full-path)
        (message "File already exists!")
      (find-file full-path)
      (message "Created and opened file: %s" full-path))))

;;=============================================================================================================
;;-------------------------------------------------------
;; custom function to switch theme after deactivating previous theme
;; =======================================================

(defun switch-theme (theme)
  "Disable current themes and load the new THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  ;; Disable all active themes
  (mapc #'disable-theme custom-enabled-themes)
  ;; Load the new theme
  (load-theme theme t))

;;=============================================================================================================
;;-------------------------------------------------------
;; custom function to remove above empty line and under empty line
;; =======================================================
(defun remove-surrounding-empty-lines ()
  "Remove the empty line above and/or below the point."
  (interactive)
  (save-excursion
    ;; Remove the empty line above the point if it exists
    (forward-line -1)
    (when (looking-at-p "^[[:space:]]*$")
      (kill-whole-line)
      (forward-line -1))
    
    ;; Remove the empty line below the point if it exists
    (forward-line 2)
    (when (looking-at-p "^[[:space:]]*$")
      (kill-whole-line))))

;;=============================================================================================================

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
    (my-modal-enter-insert-mode)
   ))

(defun my-delete-line-start ()
  "Delete from cursor to start of line."
  (interactive)
  (progn
    (delete-region (line-beginning-position) (point))
    (my-modal-enter-insert-mode)
  ))

(defun delete-buffer-content ()
  "Delete the entire content of the current buffer."
  (interactive)
  (progn
    (delete-region (point-min) (point-max))
    (my-modal-enter-insert-mode)
  ))

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
      ))
  (my-modal-enter-insert-mode))

(defun my-delete-word-backward-safe ()
  "Safely delete the word before point."
  (interactive)
  (if (or buffer-read-only (bobp)) ;; Check if buffer is read-only or at the beginning
      (message "Cannot delete: buffer is read-only or at beginning of buffer")
    (let ((start (point)))
      (backward-word)
      (delete-region (point) start)
      )))

(defun my-delete-word-forward-safe ()
  "Safely delete the word after point."
  (interactive)
  (if (or buffer-read-only (eobp)) ;; Check if buffer is read-only or at the end
      (message "Cannot delete: buffer is read-only or at end of buffer")
    (let ((start (point)))
      (forward-word)
      (delete-region start (point))
      )))
      
      
(defun my-delete-extended-string ()
  "Delete an extended string at point based on context and enter insert mode."
  (interactive)
  (cond
   ;; Inside quotes
   ((thing-at-point 'string)
    (let ((bounds (bounds-of-thing-at-point 'string)))
      (delete-region (car bounds) (cdr bounds))))
   
   ;; Inside parentheses
   ((thing-at-point 'list)
    (let ((bounds (bounds-of-thing-at-point 'list)))
      (delete-region (car bounds) (cdr bounds))))
   
   ;; URL or file path (no spaces)
   ((thing-at-point 'url)
    (let ((bounds (bounds-of-thing-at-point 'url)))
      (delete-region (car bounds) (cdr bounds))))
   
   ;; Symbol (variable names, function names, etc.)
   ((thing-at-point 'symbol)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (delete-region (car bounds) (cdr bounds))))
   
   ;; Default: delete non-whitespace sequence
   (t
    (let ((start (point))
          (end (save-excursion
                 (skip-chars-forward "^ \t\n\r")
                 (point))))
      (delete-region start end))))
  
  ;; Enter insert mode after deletion
  (my-modal-enter-insert-mode))



;;======== deletions through visual mode


;; keybiding "d" in visual mode

 (defun my-visual-delete ()
  "Delete selected region, save to kill ring, and return to insert mode."
  (interactive)
  (when (region-active-p)
    (kill-region (region-beginning) (region-end)))
  (deactivate-mark)
  (my-modal-enter-normal-mode)) ;; with this all deletes ends in the normal mode

;; extra function for visual di, I want this one to end in insert-mode
(defun visual-between-equal-chars-delete-and-insert ()
  "Select between equal chars, delete selection, and enter insert mode."
  (interactive)
  (visual-between-equal-chars)
  ;; Now implement the delete part but enter insert mode
  (when (region-active-p)
    (kill-region (region-beginning) (region-end)))
  (deactivate-mark)
  (my-modal-enter-insert-mode))


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
  
  
;; ==================================================== Consult

(defun my/consult-find-sort-by-depth-rg-fast-select ()
  "Run consult-find with ripgrep (with exclusions) and sort results by path depth."
  (interactive)
  (let* ((dir (read-directory-name "Select directory: "))
         (default-directory (if (and dir (file-directory-p dir))
                               dir
                             default-directory))
         ;; Use ripgrep with common exclusions
         (rg-results (shell-command-to-string 
                     "rg --files --hidden --glob '!.git/' --glob '!node_modules/' --glob '!dist/' --glob '!build/'"))
         ;; Split into lines and sort by depth
         (sorted-results (sort (split-string rg-results "\n" t)
                              (lambda (a b)
                                (< (length (split-string a "/" t))
                                   (length (split-string b "/" t)))))))
    ;; Present the sorted results using consult
    (let ((selection (consult--read
                     sorted-results
                     :prompt "Find (rg fast): "
                     :category 'file
                     :sort nil
                     :require-match t
                     :history 'file-name-history)))
      (when selection
        (find-file selection)))))


;; new --no need to select easy for finding files in current dir

(defun my/consult-find-sort-by-depth-rg-fast ()
  "Run consult-find with ripgrep (with exclusions) and sort results by path depth."
  (interactive)
  (let* ((file-path (buffer-file-name))
         (home-dir (expand-file-name "~/"))
         ;; Get the first level directory below home
         (first-level-dir 
          (if file-path
              (let ((rel-path (file-relative-name file-path home-dir)))
                (if (string-match "^\\([^/]+\\)/" rel-path)
                    (expand-file-name (concat home-dir (match-string 1 rel-path)))
                  home-dir))
            home-dir))
         (default-directory first-level-dir)  ; Set to first level directory
         ;; Use ripgrep with common exclusions
         (rg-results (shell-command-to-string 
                     "rg --files --hidden --glob '!.git/' --glob '!node_modules/' --glob '!dist/' --glob '!build/'"))
         ;; Split into lines and sort by depth
         (sorted-results (sort (split-string rg-results "\n" t)
                              (lambda (a b)
                                (< (length (split-string a "/" t))
                                   (length (split-string b "/" t)))))))
    ;; Present the sorted results using consult
    (let ((selection (consult--read
                     sorted-results
                     :prompt "Find (rg fast): "
                     :category 'file
                     :sort nil
                     :require-match t
                     :history 'file-name-history)))
      (when selection
        (find-file selection)))))
        

(defun my/consult-dir-then-find-by-depth-rg ()
  "First select a directory with consult-dir, then find files sorted by path depth using ripgrep."
  (interactive)
  ;; Store current directory to detect change
  (let ((original-dir default-directory)
        selected-dir)
    
    ;; Call consult-dir to select a directory
    (call-interactively 'consult-dir)
    ;; Store the selected directory
    (setq selected-dir default-directory)
    
    ;; Only proceed if directory changed
    (when (not (equal original-dir selected-dir))
      (let* (;; Use ripgrep with common exclusions
             (rg-results (shell-command-to-string 
                         "rg --files --hidden --glob '!.git/' --glob '!node_modules/' --glob '!dist/' --glob '!build/'"))
             ;; Split into lines and sort by depth
             (sorted-results (sort (split-string rg-results "\n" t)
                                  (lambda (a b)
                                    (< (length (split-string a "/" t))
                                       (length (split-string b "/" t)))))))
        ;; Present the sorted results using consult
        (let ((selection (consult--read
                         sorted-results
                         :prompt "Find (rg): "
                         :category 'file
                         :sort nil   ; already sorted
                         :require-match t
                         :history 'file-name-history)))
          (when selection
            (find-file selection)))))))
            
            
;; Create a wrapper function that finds a file and opens it in a horizontal split
(defun my/find-file-split-right ()
  "Find a file using consult and open it in a new window below."
  (interactive)
  (let ((completing-read-function #'completing-read-default))
    ;; First, call the original function to get the file
    (call-interactively #'my/consult-find-sort-by-depth-rg-fast-select)
    ;; Now get the most recently visited file (which should be the one just opened)
    (let ((recent-file (buffer-file-name)))
      ;; Restore the buffer we had before finding the file
      (switch-to-buffer (other-buffer (current-buffer) 1))
      ;; Now split and open the file
      (split-window-right)
      (other-window 1)
      (find-file recent-file))))


;; ================================================================ Start Menu function **

;; Define a persistent toggle variable if not already defined
(defvar my-find-current-window t
  "When non-nil, find files in current window instead of other window.")

;; I am not using this anymore but it could be handy to toggle of the hydra's
(defvar my-hydra-visible t "Whether the hydra menu is visible.")

(defun toggle-hydra-visibility ()
  "Toggle visibility of hydra hints."
  (interactive)
  (setq my-hydra-visible (not my-hydra-visible))
  (setq hydra-is-helpful my-hydra-visible)
  (message "Hydra visibility is now %s" (if my-hydra-visible "on" "off"))
  ;; Redisplay the current hydra if one is active
  (when (bound-and-true-p hydra-curr-body-fn)
    (funcall hydra-curr-body-fn)))


;; ====================== phi search custom functions **

(defun phi-search-quick-occur ()
  "Create an occur buffer from phi-search without exiting."
  (interactive)
  (let* ((pattern (buffer-substring-no-properties 
                  (minibuffer-prompt-end) 
                  (point-max)))
         (target-buffer (cond
                        ((consp phi-search--target) (cdr phi-search--target))
                        ((bufferp phi-search--target) phi-search--target)
                        (t (current-buffer))))
         (minibuffer-window (selected-window))
         (occur-buf (get-buffer-create "*Phi-Occur*")))
    
    ;; Verify that we have a valid buffer and pattern
    (when (and pattern
               (not (string-empty-p pattern))
               (buffer-live-p target-buffer))
      
      ;; Create the occur buffer manually
      (with-current-buffer occur-buf
        (occur-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Matches for '%s' in buffer: %s\n\n"
                          pattern (buffer-name target-buffer)))
          
          ;; Manually find and format matches
          (let ((count 0))
            (with-current-buffer target-buffer
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward pattern nil t)
                  (let* ((line-num (line-number-at-pos))
                         (line-beg (line-beginning-position))
                         (line-text (buffer-substring-no-properties 
                                     line-beg (line-end-position)))
                         (match-pos (match-beginning 0)))
                    
                    (with-current-buffer occur-buf
                      (insert (format "%7d: %s\n" line-num line-text))
                      
                      ;; Add navigation properties
                      (put-text-property 
                       (line-beginning-position) (line-end-position)
                       'occur-target match-pos)
                      (put-text-property
                       (line-beginning-position) (line-end-position)
                       'occur-target-line-face 'link))
                    
                    (setq count (1+ count))))))
            
            ;; Insert count information
            (goto-char (point-min))
            (forward-line 1)
            (if (> count 0)
                (insert (format "%d matches found\n\n" count))
              (insert "No matches found\n\n"))))
        
        ;; Store the minibuffer window for returning to phi-search
        (set (make-local-variable 'phi-search-minibuffer) minibuffer-window)
        
	(defun phi-occur-return-to-phi ()
	  "Return to phi-search from occur buffer and close the occur window and buffer."
	  (interactive)
	  (when (boundp 'phi-search-minibuffer)
	    (let ((win phi-search-minibuffer)
		  (occur-buffer (current-buffer))
		  (occur-window (selected-window)))
	      (when (window-live-p win)
		(select-window win)
		(when (and (window-live-p occur-window)
		           (not (eq occur-window (frame-root-window))))
		  (delete-window occur-window))
		(kill-buffer occur-buffer)))))
        
        ;; Create a keymap with the simpler return function
        (let ((map (make-sparse-keymap)))
          ;; Navigation keys
          (define-key map (kbd "p") 'previous-line)
          (define-key map (kbd "n") 'next-line)
          (define-key map (kbd "q") 'phi-occur-return-to-phi)
          
          ;; Apply the keymap
          (use-local-map map)))
      
      ;; Force-deactivate any transient maps before switching
      (when (fboundp 'reset-this-command-lengths)
        (reset-this-command-lengths))
      
      ;; Display AND switch to the buffer
      (select-window (display-buffer occur-buf)))))
      
      
;; ========================= Multi Occur **

(defun search-functions-in-buffers ()
  "Search for function definitions across multiple buffers."
  (interactive)
  (let* ((pattern (read-string "Search for function: " "defun "))
         (buffers (completing-read-multiple 
                  "Search in buffers (RET for all code buffers): "
                  (mapcar #'buffer-name (buffer-list))
                  nil nil nil 'buffer-name-history)))
    
    ;; If no buffers selected, use all programming mode buffers
    (when (string= (car buffers) "")
      (setq buffers (mapcar #'buffer-name 
                           (seq-filter (lambda (b) 
                                       (with-current-buffer b
                                         (derived-mode-p 'prog-mode)))
                                     (buffer-list)))))
    
    ;; Run multi-occur with the function search pattern
    (multi-occur (mapcar #'get-buffer buffers) pattern)
    
    ;; Optionally customize the occur buffer's keymap for i/o navigation
    (with-current-buffer "*Occur*"
      (let ((map (copy-keymap occur-mode-map)))
        (define-key map (kbd "i") 'previous-line)
        (define-key map (kbd "o") 'next-line)
        (use-local-map map)))))


;; =========================== Prot keyboard-quit **

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;; Marks functions


(defun my/consult-mark-forward ()
  "Jump to the next mark in the local mark ring."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'set-mark-command)))

(defun my/consult-mark-backward ()
  "Jump to the previous mark in the local mark ring."
  (interactive)
  (let ((current-prefix-arg '(-4)))  ;; Negative prefix to go backward
    (call-interactively 'set-mark-command)))

(defun my/push-mark-with-feedback ()
  "Push mark at point and display a message."
  (interactive)
  (push-mark nil t)
  (message "Mark set (position %s)" (point)))

(defun my/swap-point-and-mark ()
  "Swap point and mark without activating the region."
  (interactive)
  (let ((mark-active nil))
    (exchange-point-and-mark)))

(defun my/exchange-point-and-mark ()
  "Swap point and mark without activating the region."
  (interactive)
  (exchange-point-and-mark)
    (my-modal-enter-visual-mode))

(defun my/mark-whole-buffer-keep-position ()
  "Mark the whole buffer and return to original position."
  (interactive)
  (let ((current-point (point)))
    (push-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (goto-char current-point)
    (my-modal-enter-visual-mode)))

;; Register-based mark functions
(defun my/quick-mark-set (arg)
  "Set a mark in register 0-9."
  (interactive "cSet mark in register (0-9): ")
  (point-to-register arg)
  (message "Mark set in register '%c'" arg))

(defun my/quick-mark-jump (arg)
  "Jump to mark in register 0-9."
  (interactive "cJump to register (0-9): ")
  (jump-to-register arg))


;;=================================================================================================
(provide 'custom-functions)
