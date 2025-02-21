;; Enhanced Visual Feedback Features

;; Volatile Highlights - flash changes from undo, yank, etc.
(require 'volatile-highlights)
(volatile-highlights-mode t)
(custom-set-faces
 '(vhl/default-face ((t (:background "#454545" :foreground nil)))))


;; Better visual undo feedback with undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-visualizer-diff t
      undo-tree-visualizer-timestamps t
      undo-tree-visualizer-relative-timestamps t)

;; Visual feedback for incremental search
(require 'anzu)
(global-anzu-mode +1)
(setq anzu-mode-lighter ""
      anzu-deactivate-region t
      anzu-search-threshold 1000)

;; Enhanced search highlight
(require 'symbol-overlay)
(dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
  (add-hook hook 'symbol-overlay-mode))

;; Visual hints for matching pairs
(require 'rainbow-mode)
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; Fancy delete selection flash
(require 'goggles)
(goggles-mode 1)
(setq goggles-pulse t)  ; Enable pulsing

;; Visual feedback for region operations
(custom-set-faces
 '(region ((t (:background "#44475a" :extend t))))
 '(secondary-selection ((t (:background "#3e4a5d" :extend t)))))

;; Live preview for code folding
(require 'origami)
(global-origami-mode 1)
(setq origami-show-fold-header t)

;; Enhanced visual line movement
(require 'ov)
(defun pulse-line-on-move ()
  "Pulse line on large movements."
  (let ((dist (abs (- (line-number-at-pos) (line-number-at-pos (mark))))))
    (when (> dist 10)
      (pulse-momentary-highlight-one-line (point)))))
(add-hook 'post-command-hook #'pulse-line-on-move)


;; Visual guide for indentation
(require 'highlight-indent-guides)
(setq highlight-indent-guides-method 'bitmap
      highlight-indent-guides-responsive 'top
      highlight-indent-guides-delay 0)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")

;; Visual bookmark indicators in fringe
(require 'bm)
(setq bm-marker 'bm-marker-left
      bm-highlight-style 'bm-highlight-line-and-fringe)
(custom-set-faces
 '(bm-face ((t (:background "#3d4451" :extend t))))
 '(bm-fringe-face ((t (:background "#bd93f9" :foreground "#bd93f9")))))


;; Show keystrokes in echo area immediately
(setq echo-keystrokes 0.01)

;; Better visual feedback for available keys
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5
      which-key-idle-secondary-delay 0.05)

;; Function to flash the mode line
(defun flash-mode-line ()
  "Flash the mode line for visual feedback."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;; Add visual indicator for long lines
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))

;; Highlight TODO keywords with different colors
(require 'hl-todo)
(global-hl-todo-mode 1)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#ff79c6")
        ("FIXME"  . "#ff5555")
        ("DEBUG"  . "#bd93f9")
        ("NOTE"   . "#50fa7b")))

(provide 'config-visual-feedback)
