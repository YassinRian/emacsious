;;; config-windows.el --- Advanced window configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive window management functionality using Hydra

;;; Code:

(require 'hydra)
(require 'windmove)
(require 'winner)

;; Enable winner-mode for window configuration undo/redo
(winner-mode 1)

;; ========================= Windows Hydra ================================

(defhydra hydra-windows (:hint nil :exit t)
  "
  Window Commands
  ^Navigation^        ^Sizing^           ^Split/Delete^      ^History^
  ^^^^^^^^-----------------------------------------------------------------
  _h_: Left          _+_: Enlarge       _v_: Split Vert     _u_: Undo
  _j_: Down          _-_: Shrink        _s_: Split Horiz    _r_: Redo
  _k_: Up            _>_: Widen         _d_: Delete         _b_: Balance
  _l_: Right         _<_: Narrow        _D_: Delete Others  _m_: Maximize
  
  ^Buffers^          ^Rotation^         ^Jump^              ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _f_: Find File     _}_: Rotate Right  _0-9_: Jump to     _w_: Save Layout
  _B_: Buffer List   _{_: Rotate Left   _SPC_: Next        _L_: Load Layout
  _p_: Previous      _x_: Swap          _TAB_: Cycle       _=_: Equal Size
  _n_: Next          _t_: Transpose     _a_: Ace Window    _o_: Only One
  "
  ;; Navigation
  ("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right")
  
  ;; Sizing
  ("+" enlarge-window "enlarge vertical")
  ("-" shrink-window "shrink vertical")
  (">" enlarge-window-horizontally "enlarge horizontal")
  ("<" shrink-window-horizontally "shrink horizontal")
  
  ;; Split and Delete
  ("v" split-window-right "split vertical")
  ("s" split-window-below "split horizontal")
  ("d" delete-window "delete window")
  ("D" delete-other-windows "delete others")
  
  ;; History
  ("u" winner-undo "undo")
  ("r" winner-redo "redo")
  ("b" balance-windows "balance")
  ("m" maximize-window "maximize")
  
  ;; Buffer Management
  ("f" find-file-other-window "find file")
  ("B" ibuffer-other-window "buffer list")
  ("p" previous-buffer "previous buffer")
  ("n" next-buffer "next buffer")
  
  ;; Rotation
  ("}" rotate-windows-forward "rotate forward")
  ("{" rotate-windows-backward "rotate backward")
  ("x" window-swap-states "swap windows")
  ("t" transpose-frame "transpose")
  
  ;; Jump
  ("0" winum-select-window-0-or-10 "window 0")
  ("1" winum-select-window-1 "window 1")
  ("2" winum-select-window-2 "window 2")
  ("3" winum-select-window-3 "window 3")
  ("4" winum-select-window-4 "window 4")
  ("5" winum-select-window-5 "window 5")
  ("6" winum-select-window-6 "window 6")
  ("7" winum-select-window-7 "window 7")
  ("8" winum-select-window-8 "window 8")
  ("9" winum-select-window-9 "window 9")
  ("SPC" other-window "next window")
  ("TAB" (lambda () 
           (interactive) 
           (select-window (previous-window))) "cycle window")
  ("a" ace-window "ace jump")
  
  ;; Special
  ("w" window-configuration-to-register "save layout")
  ("L" jump-to-register "load layout")
  ("=" balance-windows-area "equal size")
  ("o" delete-other-windows "only one")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c w") #'hydra-windows/body)

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
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-background nil))

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

(provide 'config-windows)
;;; config-windows.el ends here
