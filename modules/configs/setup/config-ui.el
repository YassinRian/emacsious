;;; config-ui.el --- UI configuration with ef-themes -*- lexical-binding: t -*-

;;; Commentary:
;; Clean and minimal UI configuration using ef-themes as the base theme

;;; Code:

;; ========================= Required Packages =============================

(require 'ef-themes)
(require 'all-the-icons)
(require 'rainbow-delimiters)
(require 'ligature)
(require 'dimmer)
(require 'beacon)
;; ========================= Basic UI Settings ============================

;; Disable unnecessary UI elements
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(electric-pair-mode 1)
(blink-cursor-mode -1)  ; No blinking cursor  

;; Basic visual settings
(set-fringe-mode 10)                    ; Give some breathing room
(add-to-list 'default-frame-alist 
             '(fullscreen . maximized))  ; Start maximized

;; Window divider settings
(setq window-divider-default-right-width 1
      window-divider-default-places 'right-only)
(window-divider-mode 1)

;; =========== Top padding trucje

;; Define header line face that inherits background from theme
(defface my-empty-header-line
  '((t :inherit default :height 0.5))
  "Face for empty header line matching current theme background.")

(defun my/empty-header-line ()
  "Create an empty header line for padding."
  (propertize " " 'face 'my-empty-header-line))

(defun my/update-header-line-background ()
  "Update header line background to match current theme."
  (when (and (boundp 'custom-enabled-themes)
             (car custom-enabled-themes))
    (let ((bg (face-background 'default)))
      (set-face-attribute 'my-empty-header-line nil 
                         :background bg)
      (set-face-attribute 'header-line nil
                         :background bg))))

;; Set the header line format
(setq-default header-line-format '(:eval (my/empty-header-line)))

;; Add to ef-themes hook to update when theme changes
(add-hook 'ef-themes-post-load-hook #'my/update-header-line-background)

;; Run once to initialize
(add-hook 'after-init-hook #'my/update-header-line-background)

;; ================= Padding algemeen

;; Add internal border padding to the Emacs frame
(set-frame-parameter nil 'internal-border-width 8)

;; If you're using display-line-numbers-mode, you can add left margin
(setq-default display-line-numbers-width 3)  ; Adjust number width for more/less padding

;; For additional left and right margins in all windows
(setq-default left-margin-width 2
              right-margin-width 2)

;; Make sure to adjust margins for all windows
(defun my/adjust-window-margins ()
  (set-window-margins nil left-margin-width right-margin-width))

;; Apply margins when window configuration changes
(add-hook 'window-configuration-change-hook #'my/adjust-window-margins)

; ========================= Beacon Configuration =========================

;; Determine if we're in terminal
(defun my/in-terminal-p ()
  "Return t if we're running in a terminal."
  (not (display-graphic-p)))

;; Configure beacon based on display type
(defun my/configure-beacon ()
  "Configure beacon settings based on display type."
  (if (my/in-terminal-p)
      ;; Terminal settings
      (setq beacon-size 10  ; Smaller size for terminal
            beacon-blink-duration 0.2  ; Faster blink
            beacon-blink-delay 0
            beacon-color "magenta"  ; Use basic color name for better terminal compatibility
            beacon-background-color nil)  ; Let terminal handle background
    ;; GUI settings
    (setq beacon-size 25
          beacon-blink-duration 0.3
          beacon-blink-delay 0.1
          beacon-color (ef-themes-get-color-value 'magenta-faint))))

;; Update beacon configuration when theme changes
(defun my/update-beacon-config ()
  "Update beacon configuration."
  (my/configure-beacon))

;; Initial configuration
(my/configure-beacon)

;; Update when theme changes
(add-hook 'ef-themes-post-load-hook #'my/update-beacon-config)

;; Enable beacon mode
(beacon-mode 1)

;; Advise movement commands to trigger beacon
(defun my/pulse-line (&rest _)
  "Pulse the current line using beacon."
  (beacon-blink))

;; Add advice to movement commands
(dolist (command '(other-window windmove-up windmove-down windmove-left windmove-right
                  scroll-up-command scroll-down-command
                  recenter-top-bottom move-to-window-line-top-bottom
                  beginning-of-buffer end-of-buffer
                  next-line previous-line))
  (advice-add command :after #'my/pulse-line))

;; ========================= Theme Configuration =========================

;; Theme settings
(setq ef-themes-mixed-fonts t
      ef-themes-variable-pitch-ui nil)  ; Disable variable-pitch for UI
      
;; Load dark theme by default
(load-theme 'ef-night t)


;; Convenient theme switching
(defun my/cycle-ef-themes ()
  "Cycle between ef-themes variants."
  (interactive)
  (let* ((themes '(ef-dark ef-light ef-night ef-day ef-winter ef-summer))
         (current (car custom-enabled-themes))
         (next (or (cadr (member current themes)) (car themes))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme next t)
    (message "Loaded theme: %s" next)))

(global-set-key (kbd "<f5>") #'my/cycle-ef-themes)

;; ========================= Font Configuration ==========================

;; Set default font
(set-face-attribute 'default nil 
                    :family "JetBrainsMono Nerd Font" 
                    :height 120 
                    :weight 'regular)
                    
;; Fixed and variable pitch fonts
(set-face-attribute 'fixed-pitch nil 
                    :family "JetBrainsMono Nerd Font"
                    :height 120)
(set-face-attribute 'variable-pitch nil 
                    :family "Cantarell" 
                    :height 120
                    :weight 'regular)

;; Enable ligatures for programming
(ligature-set-ligatures 't '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
                            ":::" ":=" "!!" "!=" "!==" "->" "->>" "-<" "-<<"
                            "=>" "=>>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-"
                            ">>" ">>-" ">>=" ">>>" "<|" "<|>" "<$" "<$>" "<+"
                            "<+>" "<!--" "<-" "<--" "<->" "<<=" "<<<" "<~" "<~~"
                            "~@" "~-" "~>" "~~" "~~>" "%%"))
(global-ligature-mode t)

;; ========================= Programming Enhancements ====================

;; Rainbow delimiters (with ef-themes colors)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Update rainbow-delimiters colors when theme changes
(defun my/update-rainbow-delimiters-colors ()
  "Update rainbow-delimiters colors based on current ef-theme."
  (custom-set-faces
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,(ef-themes-get-color-value 'magenta-warmer)))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,(ef-themes-get-color-value 'blue)))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,(ef-themes-get-color-value 'red-warmer)))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,(ef-themes-get-color-value 'cyan)))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,(ef-themes-get-color-value 'yellow)))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,(ef-themes-get-color-value 'green)))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,(ef-themes-get-color-value 'purple)))))))

;; Update colors when theme changes
(add-hook 'ef-themes-post-load-hook #'my/update-rainbow-delimiters-colors)

;; ========================= Window Focus ================================

;; Dimmer for inactive windows
(setq dimmer-fraction 0.3)
(dimmer-configure-which-key)
(dimmer-mode t)

;; Enhanced visual feedback
(setq show-paren-style 'mixed
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;; Smoother scrolling
(setq scroll-margin 0
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil)
      
      
;;========================================= Alacritty ================================
           
(defun get-current-theme-bg ()
  "Get the current theme's background color."
  (face-background 'default))

(defun update-alacritty-colors ()
  "Update Alacritty colors based on current Emacs theme."
  (interactive)
  (let* ((default-bg "#1C2F45")  ; Define your default background color
         (theme-bg (get-current-theme-bg))
         (bg-color (if (or (null theme-bg)
                           (string-match "nspecified-bg" theme-bg))
                       default-bg
                     theme-bg))
         (config-file "~/.config/alacritty/alacritty.toml")
         (temp-file "/tmp/alacritty-temp.toml")
         (color-format (substring bg-color 1)))  ; Remove # from color
    
    (with-temp-file temp-file
      (insert-file-contents config-file)
      (goto-char (point-min))
      (while (re-search-forward "background = \"0x[0-9A-Fa-f]+\"" nil t)
        (replace-match (format "background = \"0x%s\"" color-format))))
    
    (rename-file temp-file config-file t)))



;; Add this to ef-themes hooks
(add-hook 'ef-themes-post-load-hook #'update-alacritty-colors)
(add-hook 'after-init-hook #'update-alacritty-colors)

(provide 'config-ui)
;;; config-ui.el ends here
