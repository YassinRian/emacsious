;;; -*- lexical-binding: t -*-

;; Couple of handy function

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
  (when (equal (buffer-name) "*eshell*")
    (boon-set-insert-state)))

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
  (split-window-below))

(defun my-split-window-right ()
  (interactive)
  (split-window-right))

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
(advice-add 'other-window :after '(lambda (&rest args)
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
;;-------------------------------------------------------
;; space function changes to boon-set-command-state
;; =======================================================



;;=================================================================================================
(provide 'custom-functions)
