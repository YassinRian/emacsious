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

;; Function to update beacon color based on current ef-theme
;; (defun my/update-beacon-color ()
;;   "Update beacon color using current ef-theme colors."
;;   (setq beacon-color (ef-themes-get-color-value 'fg-dim)))  ; Using a subtle magenta

;; Configure beacon based on display type
(defun my/configure-beacon ()
  "Configure beacon settings based on display type."
  (if (my/in-terminal-p)
      ;; Terminal settings
      (setq beacon-size 10  ; Smaller size for terminal
            ;;beacon-blink-duration 0.5  
            beacon-blink-delay 0
            beacon-color (ef-themes-get-color-value 'fg-dim)
            beacon-background-color nil)  ; Let terminal handle background
    ;; GUI settings
    (setq beacon-size 25
          beacon-blink-duration 0.3
          beacon-blink-delay 0.1
          beacon-color (ef-themes-get-color-value 'fg-dim))))

(my/configure-beacon)


;; Update color when theme changes
(add-hook 'ef-themes-post-load-hook #'my/configure-beacon)

;; Enable beacon
(beacon-mode t)

;; Clear any existing advice
;;(advice-remove 'next-line #'my/pulse-line)
;;(advice-remove 'previous-line #'my/pulse-line)

;; Minimal pulse function
(defun my/pulse-line (&rest _)
  (beacon-blink))

;; Only advise vertical movement
(advice-add 'next-line :after #'my/pulse-line)
(advice-add 'previous-line :after #'my/pulse-line)


;; Disable cursor blinking in all scenarios
(blink-cursor-mode -1)
(setq visible-cursor nil)
(setq blink-cursor-blinks 0)
(setq blink-cursor-interval 0)
(setq blink-cursor-delay 0)

;; Add terminal-specific commands to disable cursor blinking
(defun disable-terminal-cursor-blinking ()
  "Disable cursor blinking in terminal"
  (when (not (display-graphic-p))
    ;; Send terminal escape sequences to disable blinking
    (send-string-to-terminal "\e[?12l")
    (send-string-to-terminal "\e[?25h")
    
    ;; For some terminals, we need to reset cursor style
    (send-string-to-terminal "\e[0 q")  ;; Reset to default
    (send-string-to-terminal "\e[2 q")  ;; Set to steady block
    ))

;; Run when Emacs starts
(disable-terminal-cursor-blinking)

;; Run when creating new frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (disable-terminal-cursor-blinking))))


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
           
(defun get-current-theme-colors ()
  "Get the current theme's background and foreground colors."
  (list (face-background 'default)
        (face-foreground 'default)))

(defun update-alacritty-colors ()
  "Update Alacritty colors based on current Emacs theme."
  (interactive)
  (let* ((default-bg "#1C2F45")    ; Define your default background color
         (default-fg "#FFFFFF")     ; Define your default foreground color
         (theme-colors (get-current-theme-colors))
         (theme-bg (nth 0 theme-colors))
         (theme-fg (nth 1 theme-colors))
         (bg-color (if (or (null theme-bg)
                          (string-match "nspecified-bg" theme-bg))
                      default-bg
                    theme-bg))
         (fg-color (if (or (null theme-fg)
                          (string-match "nspecified-fg" theme-fg))
                      default-fg
                    theme-fg))
         (config-file "~/.config/alacritty/alacritty.toml")
         (temp-file "/tmp/alacritty-temp.toml")
         (bg-color-format (substring bg-color 1))
         (fg-color-format (substring fg-color 1)))
    
    (with-temp-file temp-file
      (insert-file-contents config-file)
      (goto-char (point-min))
      ;; Update background color
      (while (re-search-forward "background = \"0x[0-9A-Fa-f]+\"" nil t)
        (replace-match (format "background = \"0x%s\"" bg-color-format)))
      ;; Update foreground color
      (goto-char (point-min))
      (while (re-search-forward "foreground = \"0x[0-9A-Fa-f]+\"" nil t)
        (replace-match (format "foreground = \"0x%s\"" fg-color-format))))
    
    (rename-file temp-file config-file t)))



;; Add this to ef-themes hooks
(add-hook 'ef-themes-post-load-hook #'update-alacritty-colors)
(add-hook 'after-init-hook #'update-alacritty-colors)

;; =================== window distinction ==============================

(defun my-set-window-divider ()
  "Set window divider color to match ef-themes border with reduced opacity."
  (let ((border-color (ef-themes-get-color-value 'border)))
    ;; Make the border more subtle by using the background color mixed with border
    (let ((subtle-color (ef-themes-get-color-value 'bg-inactive)))
      (set-face-background 'vertical-border subtle-color)
      (set-face-foreground 'vertical-border subtle-color))))

;; For theme switches
(add-hook 'ef-themes-post-load-hook #'my-set-window-divider)
;; For initial load
(add-hook 'after-init-hook #'my-set-window-divider)



(provide 'config-ui)
;;; config-ui.el ends here
