;;; config-windows.el --- Advanced window configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive window management functionality using Hydra

;;; Code:

(require 'pretty-hydra)
(require 'windmove)
(require 'winner)

;; Enable winner-mode for window configuration undo/redo
(winner-mode 1)

;; ========================= Windows Hydra ================================

(pretty-hydra-define hydra-windows (:title "Window Commands" :quit-key "q" :exit t)
  (
   
   ;; Sizing column
   "Sizing"
    (("+" enlarge-window "Enlarge" :exit nil)
      ("-" shrink-window "Shrink" :exit nil)
      (">" enlarge-window-horizontally "Widen" :exit nil)
      ("<" shrink-window-horizontally "Narrow" :exit nil)
      ("=" balance-windows-area "Equal Size")
      )
   
   ;; Split/Delete column
    "Split/Delete"
    (("v" split-window-right "Split Vert")
     ("s" split-window-below "Split Horiz")
     ("d" delete-window "Delete")
     ("D" delete-other-windows "Delete Others"))
   
   ;; History column
   "History"
    (("u" winner-undo "Undo")
     ("r" winner-redo "Redo")
     ("b" balance-windows "Balance")
     ("m" maximize-window "Maximize"))
   
   ;; Rotation column
   "Rotation"
    (("}" rotate-windows-forward "Rotate Right")
     ("{" rotate-windows-backward "Rotate Left")
     ("x" window-swap-states "Swap")
     ("t" transpose-frame "Transpose"))
   
   ;; Jump column
   "Jump"
    (
     ("SPC" other-window "Next window")
     ("TAB" (lambda () 
              (interactive) 
              (select-window (previous-window))) "Cycle window")
     ("a" ace-window "Ace Jump"))
   
   ;; Special column
   "Bookmark Wins"
   (
    ("b" burly-bookmark-windows "Save windows")
    ("l" burly-open-bookmark "Load window")
    ("q" nil "Exit" :exit t)
     )))
     
  
  
;; ========================= Helper Functions ============================

(defun rotate-windows-forward ()
  "Rotate windows clockwise."
  (interactive)
  (if (= (count-windows) 2)
      (window-swap-states)
    (let* ((windows (window-list))
           (configs (mapcar #'window-configuration-to-register windows))
           (num-windows (length windows)))
      (dotimes (i (- num-windows 1))
        (let* ((index (+ i 1))
               (source (elt windows index))
               (target (elt windows i)))
          (window-swap-states source target))))))

(defun rotate-windows-backward ()
  "Rotate windows counter-clockwise."
  (interactive)
  (if (= (count-windows) 2)
      (window-swap-states)
    (let* ((windows (reverse (window-list)))
           (configs (mapcar #'window-configuration-to-register windows))
           (num-windows (length windows)))
      (dotimes (i (- num-windows 1))
        (let* ((index (+ i 1))
               (source (elt windows index))
               (target (elt windows i)))
          (window-swap-states source target))))))

;; ========================= Window Configuration ======================

;; Basic window settings
(setq split-height-threshold nil      ; More natural split behavior
      split-width-threshold 160       ; Split vertically if width > 160
      window-combination-resize t     ; Resize windows proportionally
      even-window-sizes 'height-only) ; Only even out window heights

;; Window numbering
(use-package winum
  :config
  (winum-mode))

;; Ace window for quick window selection
(use-package ace-window
  :config (setq
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-scope 'frame
           aw-background t)
          (custom-set-faces
 '(aw-leading-char-face ((t :inherit (bold) 
                           :height 4.0 
                           :foreground "red"
                           :background "gray10"
                           :box (:line-width -1 :color "gray50" :style released-button)
                           :weight extra-bold)))) 
           
          )

;; Save window configurations
(defun save-window-layout (register)
  "Save current window layout to REGISTER."
  (interactive "cSave window configuration to register: ")
  (window-configuration-to-register register)
  (message "Window configuration saved to register '%c'" register))

(defun load-window-layout (register)
  "Restore window layout from REGISTER."
  (interactive "cRestore window configuration from register: ")
  (jump-to-register register))
  

  

  
;; ============================== activities =================================

(use-package activities
 :init
 (activities-mode)
 ;;(activities-tabs-mode)
 ;; Prevent `edebug' default bindings from interfering.
 (setq edebug-inhibit-emacs-lisp-mode-bindings t)
)

(provide 'config-windows)
;;; config-windows.el ends here
