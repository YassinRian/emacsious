(provide 'my-packages)

;; Load Elpaca
(require 'elpaca)


;; Install & Configure Completion Frameworks
(elpaca hydra)
(elpaca pretty-hydra)
(elpaca bind-key)
(elpaca boon)

;; Install & Configure Editing Tools
(elpaca which-key) ;; Show keybinding hints
(elpaca vundo)
(elpaca undo-fu)
;; =====================  UI Enhancements =================================

;; Themes and Modeline
(elpaca ef-themes)


;; Navigation
(elpaca avy)

;; Icons and Fonts
(elpaca all-the-icons)      ;; Icons for files and buffers
(elpaca nerd-icons)         ;; Nerd fonts icons
(elpaca kind-icon)
(elpaca svg-lib)
(elpaca nerd-icons-corfu)
(elpaca nerd-icons-dired)
(elpaca nerd-icons-completion)

;; Window Management
(elpaca winum)
(elpaca ace-window)
(elpaca activities)
(elpaca burly)

;; Smooth Scrolling
(elpaca good-scroll)        ;; Smooth scrolling for Emacs
(elpaca smooth-scrolling)

;; Visual Tweaks
(elpaca highlight-indent-guides) ;; Show indentation levels
(elpaca rainbow-delimiters)      ;; Colorful parentheses/braces/brackets
(elpaca beacon)                  ;; Highlight cursor movement
(elpaca highlight-quoted)
(elpaca diff-hl)
(elpaca dimmer)
(elpaca solaire-mode)
(elpaca ligature)
;;(elpaca rainbow-mode)

;; Visual feedback
(elpaca volatile-highlights)
(elpaca anzu)
(elpaca symbol-overlay)
(elpaca rainbow-mode)
(elpaca highlight-parentheses)
(elpaca goggles)
(elpaca origami)
(elpaca bm)
(elpaca hl-todo)
(elpaca ov)
(elpaca minimap)

;; Completion Company
(elpaca company)
(elpaca company-fuzzy)
(elpaca company-box)
(elpaca company-quickhelp)
(elpaca company-prescient)
(elpaca yasnippet)
(elpaca tempel)

;; search
(elpaca consult)
(elpaca consult-dir
  :config
  (setq consult-dir-default-command #'consult-dir-dired))
(elpaca phi-search)
(elpaca occur-context-resize
   ;; Custom navigation in the occur buffer
   (define-key occur-mode-map (kbd "i") 'occur-prev)
   (define-key occur-mode-map (kbd "o") 'occur-next))
(elpaca affe)
(elpaca wgrep)
(elpaca deadgrep)
(elpaca embark)
(elpaca embark-consult)

;; Completion Corfu
(elpaca corfu)
(elpaca orderless)
(elpaca cape)
(elpaca corfu-terminal)
;;(elpaca corfu-quick)

;; vertico
(elpaca vertico)
(elpaca marginalia)

;; Python config
(elpaca pyenv-mode)
(elpaca pyvenv)
(elpaca lsp-mode)
(elpaca lsp-pyright)
(elpaca lsp-ui)
(elpaca python-black)
(elpaca python-pytest)
(elpaca poetry)
(elpaca projectile)

;; Viper
(autoload 'viper-ex "viper")



;; ========================== General Package Settings ===============================================



;; ========================== Elpaca wait

(elpaca-wait)    

;; ======================================= Load custom configurations =================================
;; loaded after the elpaca packages

(add-to-list 'load-path (expand-file-name "modules/configs/utilities" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/configs/downloaded_packages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/configs/setup" user-emacs-directory))


;; Only loading what is needed at startup, other packages are required where they are needed (see other files)

(require 'config-ui)
(require 'config-scroll)
(require 'config-completion-corfu)
(require 'config-python-corfu)
(require 'config-vertico)
(require 'config-search)
(require 'config-dired)
(require 'config-windows)
(require 'config-marks)
(require 'config-text-manipulation)



