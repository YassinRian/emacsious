;; config-scroll.el - Enhanced scrolling configuration


;; Smooth scrolling configuration

;; Enable pixel-based scrolling for smoother movement
(pixel-scroll-mode 1)

;; Make scrolling more smooth in general
(setq scroll-margin 3
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1))  ; one line at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
      mouse-wheel-follow-mouse 't                   ; scroll window under mouse
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; Enable smooth scrolling package if available
(when (require 'smooth-scrolling nil 'noerror)
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 3))

;; Alternative smooth scrolling implementation
(require 'view)
(setq view-scroll-auto-exit nil)

;; Improved scrolling behavior for large files
(setq-default bidi-display-reordering nil)  ; Disable bidirectional text scanning
(setq-default bidi-paragraph-direction 'left-to-right)

;; Optional: Enable good-scroll package if installed
(when (require 'good-scroll nil 'noerror)
  (good-scroll-mode 1))

;; Adjust GC threshold during scrolling for better performance
(setq gc-cons-threshold 100000000)  ; Set to ~100MB
(add-hook 'focus-out-hook #'garbage-collect)


(require 'minimap)

;; Scroll-sensitive minimap
(defvar my-minimap-timer nil
  "Timer to hide minimap after scrolling.")

(defun my-show-minimap ()
  "Show minimap and set timer to hide it."
  (interactive)
  (when (require 'minimap nil 'noerror)
    (unless minimap-mode
      (minimap-mode 1))
    ;; Reset timer if it exists
    (when my-minimap-timer
      (cancel-timer my-minimap-timer))
    ;; Set new timer to hide minimap after 2 seconds of no scrolling
    (setq my-minimap-timer
          (run-with-timer 2 nil
                         (lambda ()
                           (when minimap-mode
                             (minimap-mode -1)))))))

;; Advice scroll commands to show minimap
(advice-add 'scroll-up-command :before #'my-show-minimap)
(advice-add 'scroll-down-command :before #'my-show-minimap)
(advice-add 'scroll-up-line :before #'my-show-minimap)
(advice-add 'scroll-down-line :before #'my-show-minimap)

;; Configure minimap appearance
(with-eval-after-load 'minimap
  (setq minimap-window-location 'right
        minimap-width-fraction 0.15
        minimap-minimum-width 20))

;; Configure minimap appearance
(with-eval-after-load 'minimap
  (setq minimap-window-location 'right
        minimap-width-fraction 0.15
        minimap-minimum-width 20))

;; Optional: Enable pixel-based scrolling precision (Emacs 29+)
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))



(provide 'config-scroll)
