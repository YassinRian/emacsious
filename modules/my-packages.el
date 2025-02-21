(provide 'my-packages)

;; Load Elpaca
(require 'elpaca)


;; Install & Configure Completion Frameworks
(elpaca hydra)
(elpaca pretty-hydra)
(elpaca boon)
(elpaca bind-key)

;; Install & Configure Editing Tools
(elpaca undo-tree) ;; Persistent undo history
(elpaca which-key) ;; Show keybinding hints

;; =====================  UI Enhancements =================================

;; Themes and Modeline
(elpaca ef-themes)

;; Icons and Fonts
(elpaca all-the-icons)      ;; Icons for files and buffers
(elpaca nerd-icons)         ;; Nerd fonts icons
(elpaca kind-icon)
(elpaca svg-lib)
(elpaca nerd-icons-corfu)

;; Window Management
(elpaca centaur-tabs)       ;; Tab bar for buffers
(elpaca popper)             ;; Pop-up management for Emacs

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
(elpaca affe)
(elpaca wgrep)
(elpaca deadgrep)

;; Completion Corfu
(elpaca corfu)
(elpaca orderless)
(elpaca cape)
(elpaca corfu-terminal)

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

;; =====================  // UI Enhancements =================================


;; Hooks for lazy loading
(elpaca-wait)



;; Load custom configurations
(add-to-list 'load-path (expand-file-name "modules/configs/utilities" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/configs/downloaded_packages" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/configs/setup" user-emacs-directory))


;; Only loading what is needed at startup, other packages are required where they are needed (see other files)
(require 'config-ui)
(require 'config-scroll)
(require 'config-visual-feedback)
(require 'config-completion-corfu)
(require 'config-python-corfu)
(require 'config-vertico)
(require 'config-search)



