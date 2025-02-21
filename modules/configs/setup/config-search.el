;;; config-search.el --- Advanced search configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive search functionality using Hydra

;;; Code:

(require 'consult)
(require 'affe)
(require 'wgrep)
(require 'deadgrep)
(require 'hydra)
(require 'pretty-hydra)
(require 'search-help)

;; ========================= Search Hydra ================================

(pretty-hydra-define hydra-search
  (:foreign-keys run
   :post (my-modal-enter-normal-mode)
   :exit t
   :title "Search Commands")
  ("Project" ; Column name
   (("p" consult-ripgrep "Project Text")
    ("g" deadgrep "Ripgrep UI")
    ("f" consult-find "Find File")
    ("F" affe-find "Fuzzy Find"))
   
   "Buffer" ; Column name
   (("s" consult-line "Current")
    ("b" consult-buffer "Buffers")
    ("a" consult-line-multi "All Buffers")
    ("w" (lambda () 
           (interactive)
           (switch-to-buffer-other-window (current-buffer))
           (consult-line)) "Other Window"))
   
   "Advanced" ; Column name
   (("i" consult-imenu "Imenu")
    ("I" consult-imenu-multi "Imenu All")
    ("o" consult-outline "Outline")
    ("l" consult-goto-line "Goto Line"))
   
   "Other" ; Column name
   (("d" (lambda () 
           (interactive)
           (let ((default-directory 
                   (read-directory-name "Search in directory: ")))
             (consult-find))) "Directory")
    ("r" consult-recent-file "Recent Files")
    ("m" consult-mark "Marks")
    ("G" consult-git-grep "Git Grep"))
    
    "Help & Exit" ; see description of functions
    (("?" my/show-search-help "Show Help")
     ("h" my/show-search-help "Show Help")
     ("q" nil "Quit"))
     ))

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

(provide 'config-search)
;;; config-search.el ends here
