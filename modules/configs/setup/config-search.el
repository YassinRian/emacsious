;;; config-search.el --- Advanced search configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive search functionality using Hydra

;;; Code:

(require 'consult)
(require 'embark)
(require 'embark-consult)
(require 'affe)
(require 'wgrep)
(require 'deadgrep)
(require 'hydra)
(require 'pretty-hydra)
(require 'search-help)

;; ========================= Search Configuration =====================

;; Ripgrep configuration
(setq consult-ripgrep-command "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --hidden -g !.git/ .")

;; Configure consult for better search experience
(setq consult-narrow-key "<"              ; Narrow key for filtering
      consult-line-numbers-widen t        ; Widen line numbers
      consult-async-min-input 2           ; Start async search after 2 chars
      consult-async-refresh-delay 0.15    ; Delay before updating async search
      consult-async-input-throttle 0.2    ; Throttle async search input
      consult-async-input-debounce 0.1)   ; Debounce async search input

;; Configure wgrep for editable grep buffers
(setq wgrep-auto-save-buffer t          ; Auto-save after applying changes
      wgrep-change-readonly-file t)     ; Allow editing read-only files

;; Configure affe for better fuzzy finding
(setq affe-regexp-function #'orderless-pattern-compiler
      affe-highlight-function #'orderless-highlight-matches)
      
;;========================================================================================================================

(defvar my-search-include-hidden nil
  "When non-nil, include hidden files and directories in searches.")

(defvar my-search-use-regexp nil
  "When non-nil, treat search queries as regular expressions.")

(defvar my-search-current-dir "~/"
  "Current directory for search operations.")

(defun my-search-select-directory ()
  "Select a directory for search operations."
  (interactive)
  (let ((dir (read-directory-name "Select search directory: " my-search-current-dir)))
    (setq my-search-current-dir (file-name-as-directory dir))
    (message "Search directory set to: %s" my-search-current-dir)))

(defun my-affe-find-files ()
  "Search for files using affe with ripgrep, with configurable options."
  (interactive)
  (let* ((hidden-args (if my-search-include-hidden "--hidden --no-ignore" "--glob=!.*/"))
         (regexp-args (if my-search-use-regexp "" "--fixed-strings"))
         (default-directory my-search-current-dir)
         (affe-find-command 
          (format "rg --color=never --files %s %s" 
                  hidden-args regexp-args)))
    (message "File search in %s with command: %s" my-search-current-dir affe-find-command)
    (let ((affe-find-command affe-find-command))
      (affe-find))))

(defun my-affe-grep ()
  "Search for text in files using affe-grep with configurable options."
  (interactive)
  (let ((default-directory my-search-current-dir)
        (affe-grep-dyn-args
         (concat (if my-search-include-hidden "--hidden --no-ignore " "--glob=!.*/ ")
                 (if my-search-use-regexp "" "--fixed-strings "))))
    (message "Text search in %s with options: %s" 
             my-search-current-dir affe-grep-dyn-args)
    ;; Use a dynamic variable to pass arguments to affe-grep
    (let ((affe-grep-dyn-args affe-grep-dyn-args))
      (affe-grep))))

(defun my-consult-project-search ()
  "Search for text in the current project using consult-ripgrep.
Uses the same hidden and regexp settings as other search functions."
  (interactive)
  ;; Construct args as a string with proper spacing
  (let* ((args (concat 
                (when my-search-include-hidden " --hidden --no-ignore")
                (unless my-search-use-regexp " --fixed-strings"))))
    
    ;; Store original args to restore later
    (let ((original-args consult-ripgrep-args))
      ;; Set the custom args for consult-ripgrep by appending our options
      (setq consult-ripgrep-args (concat original-args args))
      (message "Using ripgrep args: %s" consult-ripgrep-args)
      
      ;; Run the appropriate search based on context
      (unwind-protect
          (if (project-current)
              (consult-ripgrep (project-root (project-current)))
            (message "Not in a project. Using directory search instead.")
            (let ((current-prefix-arg '(4))) ; Force prompt for directory
              (call-interactively #'consult-ripgrep)))
        
        ;; Restore original args
        (setq consult-ripgrep-args original-args)))))
        
(defun my-search-buffer-text ()
  "Search for text in the current buffer using consult-line.
Uses regexp searching if enabled in the search options."
  (interactive)
  (if my-search-use-regexp
      (consult-line)
    (let ((consult-line-grep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --fixed-strings"))
      (consult-line))))
      
(defun my-imenu-search ()
  "Search for symbols and definitions in current buffer using consult-imenu."
  (interactive)
  (call-interactively #'consult-imenu))
  
 (defun my-symbol-search ()
  "Search for symbols/definitions across files in a project or directory.
Uses ripgrep with a pattern optimized for finding function and class definitions."
  (interactive)
  ;; Store the original value to restore later
  (let* ((original-args consult-ripgrep-args)
         ;; Create our custom arguments with proper flags and a pattern for definitions
         (custom-args (concat 
                       "rg --null --line-buffered --color=ansi --max-columns=1000 "
                       "--no-heading --line-number "
                       (when my-search-include-hidden "--hidden --no-ignore ")
                       "--pcre2 '(function|def|class|struct|enum|interface|trait|impl|module)\\s+\\w+'")))
    
    ;; Use our custom args for this function call
    (setq consult-ripgrep-args custom-args)
    (message "Symbol search with args: %s" consult-ripgrep-args)
    
    ;; Run the appropriate search based on context
    (unwind-protect
        (if (project-current)
            (consult-ripgrep (project-root (project-current)))
          (message "Not in a project. Using directory search instead.")
          (let ((current-prefix-arg '(4))) ; Force prompt for directory
            (call-interactively #'consult-ripgrep)))
      
      ;; Restore the original arguments when done
      (setq consult-ripgrep-args original-args))))
      
 
(defun my-multi-occur-search ()
  "Search for a pattern across multiple buffers using multi-occur."
  (interactive)
  (let ((buffers (consult--multi-occur-buffer-list))
        (pattern (read-regexp "Search pattern: ")))
    (when (and buffers pattern)
      (multi-occur buffers pattern my-search-use-regexp))))

(defun consult--multi-occur-buffer-list ()
  "Prompt for a list of buffers to search with multi-occur."
  (let ((buffers (mapcar #'get-buffer
                         (split-string
                          (completing-read "Choose buffers (comma-separated): "
                                           (mapcar #'buffer-name (buffer-list))
                                           nil nil nil 'buffer-name-history)
                          ", *" t))))
    (when (null buffers)
      (error "No buffers selected"))
    buffers))
            

(pretty-hydra-define hydra-search
  (:title "Search Utilities" :quit-key "q" :color pink)
  ("Options"
   (("h" (setq my-search-include-hidden (not my-search-include-hidden))
     (format "Hidden files [%s]" (if my-search-include-hidden "✓" " "))
     :toggle my-search-include-hidden)
    ("e" (setq my-search-use-regexp (not my-search-use-regexp))
     (format "Regexp [%s]" (if my-search-use-regexp "✓" " "))
     :toggle my-search-use-regexp)
    ("d" my-search-select-directory
     (format "Dir: %s" (abbreviate-file-name my-search-current-dir))))
   
   "Actions"
   (("f" my-affe-find-files "Find files" :exit t)
    ("g" my-affe-grep "Search in files" :exit t)
    ("p" my-consult-project-search "Project search" :exit t)
    ("l" my-search-buffer-text "Search in buffer" :exit t)
    ("u" my-ugrep-with-options "Ugrep" :exit t)
    ("i" my-imenu-search "Definitions & symbols" :exit t)
    ("s" my-symbol-search "Project symbols" :exit t)
    ("m" my-multi-occur-search "Multi-buffer search" :exit t)
    ("/" phi-replace-query "Search and Replace" :exit t)
    ("q" nil "Quit" :exit t))))



;; =========================================================== ugrep ====================================================

(defvar my-ugrep-options '("-r")
  "List of current ugrep options.")

(defun my-ugrep-toggle-option (option)
  "Toggle OPTION in `my-ugrep-options'."
  (if (member option my-ugrep-options)
      (setq my-ugrep-options (delete option my-ugrep-options))
    (add-to-list 'my-ugrep-options option))
  (message "ugrep options: %s" (string-join my-ugrep-options " ")))

(defun my-ugrep-option-p (option)
  "Return t if OPTION is enabled, nil otherwise."
  (if (member option my-ugrep-options) t nil))

(defun my-ugrep-get-filter (prefix)
  "Get filter option starting with PREFIX."
  (or (cadr (member (concat "--" prefix) my-ugrep-options))
      (let ((found nil))
        (dolist (opt my-ugrep-options)
          (when (string-prefix-p (concat "--" prefix) opt)
            (setq found (substring opt (+ 2 (length prefix))))))
        found)
      ""))
      
(defun ugrep-in-buffers (regexp)
  "Search for REGEXP in all open buffers using ugrep."
  (interactive "sSearch buffers for: ")
  (let* ((temp-dir (make-temp-file "buffers-ugrep-" t))
         (buffer-files '())
         (orig-buffers (buffer-list))
         (buffer-mappings '()))
    
    ;; Save all buffer contents to temp files
    (dolist (buf orig-buffers)
      (when (and (buffer-live-p buf)
                 (not (string-match-p "^ " (buffer-name buf))))
        (let* ((buf-name (buffer-name buf))
               (temp-file (expand-file-name 
                          (replace-regexp-in-string "[^a-zA-Z0-9_.-]" "_" buf-name)
                          temp-dir)))
          (with-current-buffer buf
            (write-region (point-min) (point-max) temp-file nil 'quiet))
          (push temp-file buffer-files)
          (push (cons temp-file buf-name) buffer-mappings))))
    
    ;; Use ugrep to search the temp files through grep interface
    (let ((default-directory temp-dir))
      (grep (concat "ugrep -n " 
                   (shell-quote-argument regexp) 
                   " " 
                   (mapconcat 'shell-quote-argument buffer-files " "))))
    
    ;; Replace temp filenames with buffer names in *grep* buffer
    (with-current-buffer "*grep*"
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\([^:\n]+\\):\\([0-9]+\\):" nil t)
          (let* ((file (match-string 1))
                 (buf-name (cdr (assoc file buffer-mappings))))
            (when buf-name
              (replace-match (concat buf-name ":\\2:") t nil))))))
    
    ;; Schedule cleanup of temp directory
    (run-with-idle-timer 1 nil (lambda () (delete-directory temp-dir t)))))

(defun ugrep-open-buffers ()
  "Run ugrep interactively on all open buffers."
  (interactive)
  (let ((default-directory (or default-directory "~/")))
    (call-interactively 'ugrep-in-buffers)))

(defun ugrep-current-directory (regexp)
  "Run ugrep for REGEXP in the current directory."
  (interactive "sSearch directory for: ")
  (let ((default-directory (file-name-directory (or buffer-file-name default-directory))))
    (grep (concat "ugrep -r -n " (shell-quote-argument regexp) " ."))))

(defun ugrep-current-project (regexp)
  "Run ugrep for REGEXP in the current project root."
  (interactive "sSearch project for: ")
  (let ((default-directory (if (fboundp 'project-root)
                             (or (ignore-errors 
                                   (project-root (project-current)))
                                 (file-name-directory (or buffer-file-name default-directory)))
                           (file-name-directory (or buffer-file-name default-directory)))))
    (grep (concat "ugrep -r -n " (shell-quote-argument regexp) " ."))))

(defun ugrep-recursive-files (regexp files)
  "Search for REGEXP in FILES recursively using ugrep."
  (interactive "sSearch for: \nsIn files (e.g., *.el): ")
  (let ((file-pattern (if (string-empty-p files) "*" files)))
    (grep (concat "ugrep -r -n --include=" 
                 (shell-quote-argument file-pattern) " "
                 (shell-quote-argument regexp) " ."))))
                 

(pretty-hydra-define hydra-ugrep (:title "Ugrep Search Options" :quit-key "q" :color pink)
  ("Options"
   (("r" (my-ugrep-toggle-option "-r") (format "Recursive [%s]" (if (my-ugrep-option-p "-r") "x" " ")))
    ("i" (my-ugrep-toggle-option "-i") (format "Ignore case [%s]" (if (my-ugrep-option-p "-i") "x" " ")))
    ("w" (my-ugrep-toggle-option "-w") (format "Whole word [%s]" (if (my-ugrep-option-p "-w") "x" " ")))
    ("l" (my-ugrep-toggle-option "-l") (format "List files only [%s]" (if (my-ugrep-option-p "-l") "x" " ")))
    ("c" (my-ugrep-toggle-option "-c") (format "Count matches [%s]" (if (my-ugrep-option-p "-c") "x" " "))))
   
   "Filters"
   (("e" (let ((ext (read-string "File extension (e.g. *.el): ")))
           (setq my-ugrep-options 
                 (cl-remove-if (lambda (opt) (string-prefix-p "--include" opt)) my-ugrep-options))
           (when (not (string-empty-p ext))
             (add-to-list 'my-ugrep-options (format "--include=%s" ext))))
         (format "File ext: %s" (my-ugrep-get-filter "include")))
    
    ("x" (let ((pattern (read-string "Exclude pattern: ")))
           (setq my-ugrep-options 
                 (cl-remove-if (lambda (opt) (string-prefix-p "--exclude" opt)) my-ugrep-options))
           (when (not (string-empty-p pattern))
             (add-to-list 'my-ugrep-options (format "--exclude=%s" pattern))))
         (format "Exclude: %s" (my-ugrep-get-filter "exclude")))
    
    ("d" (let ((depth (read-string "Max depth: ")))
           (setq my-ugrep-options 
                 (cl-remove-if (lambda (opt) (string-prefix-p "--depth" opt)) my-ugrep-options))
           (when (not (string-empty-p depth))
             (add-to-list 'my-ugrep-options (format "--depth=%s" depth))))
         (format "Depth: %s" (my-ugrep-get-filter "depth")))
    
    ("t" (let ((type (read-string "File type (e.g. elisp): ")))
           (setq my-ugrep-options 
                 (cl-remove-if (lambda (opt) (string-prefix-p "--type" opt)) my-ugrep-options))
           (when (not (string-empty-p type))
             (add-to-list 'my-ugrep-options (format "--type=%s" type))))
         (format "Type: %s" (my-ugrep-get-filter "type"))))
   
   "Actions"
   (("s" (progn
           (call-interactively 'consult-dir)
           (let* ((pattern (read-string "ugrep pattern: " nil 'grep-history))
                  (command (format "ug --color=always -n %s %s ." 
                                  (string-join my-ugrep-options " ")
                                  (shell-quote-argument pattern))))
             (grep command)))
         "Search")
    ("p" (setq my-ugrep-options '("-r")) "Reset options")
    ("q" nil "Quit"))))

(defun my-ugrep-with-options ()
  "Run ugrep with options selected via pretty-hydra."
  (interactive)
  (hydra-ugrep/body))
  

;; ============================================================ EOF =======================================================

(provide 'config-search)
;;; config-search.el ends here
