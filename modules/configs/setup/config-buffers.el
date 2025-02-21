;;; config-buffers.el --- Advanced buffer configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive buffer functionality using Hydra

;;; Code:

(require 'hydra)

;; ========================= Buffer Hydra ================================

(defhydra hydra-buffers (:hint nil :exit t)
  "
  Buffer Commands
  ^Basic^            ^Save/Kill^        ^Window^           ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _n_: Next          _s_: Save          _v_: Split Vert    _r_: Revert
  _p_: Previous      _S_: Save All      _h_: Split Horiz   _R_: Rename
  _l_: List          _k_: Kill          _o_: Other         _N_: New
  _b_: Switch        _K_: Kill Others   _d_: Delete Win    _E_: Eval
  
  "
  ;; Basic buffer operations
  ("n" next-buffer "next buffer")
  ("p" previous-buffer "previous buffer")
  ("l" ibuffer "list buffers")
  ("b" switch-to-buffer "switch buffer")
  
  ;; Save and Kill operations
  ("s" save-buffer "save buffer")
  ("S" save-some-buffers "save all buffers")
  ("k" kill-buffer "kill buffer")
  ("K" kill-other-buffers "kill other buffers")
  
  ;; Window operations
  ("v" split-window-right "split vertical")
  ("h" split-window-below "split horizontal")
  ("o" other-window "other window")
  ("d" delete-window "delete window")
  
  ;; Special operations
  ("r" revert-buffer "revert buffer")
  ("R" rename-buffer "rename buffer")
  ("N" (lambda () 
         (interactive)
         (switch-to-buffer (generate-new-buffer "untitled")))
   "new buffer")
  ("E" eval-buffer "eval buffer")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Helper functions
(defun kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c b") #'hydra-buffers/body)

;; ========================= Buffer Configuration =====================

;; IBuffer configuration
(setq ibuffer-expert t                    ; Don't ask for confirmation
      ibuffer-show-empty-filter-groups nil ; Don't show empty filter groups
      ibuffer-saved-filter-groups         ; Define filter groups
      '(("default"
         ("Emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")))
         ("Config" (filename . ".emacs.d"))
         ("Org" (mode . org-mode))
         ("Code" (or (mode . python-mode)
                     (mode . emacs-lisp-mode)
                     (mode . c-mode)
                     (mode . rust-mode)))
         ("Dired" (mode . dired-mode))
         ("Help" (or (name . "\\*Help\\*")
                     (name . "\\*Apropos\\*")
                     (name . "\\*info\\*"))))))

;; Buffer naming
(setq uniquify-buffer-name-style 'forward ; Unique buffer names with path
      uniquify-separator "/"             ; Separator in buffer names
      uniquify-after-kill-buffer-p t     ; Update unique names after killing
      uniquify-ignore-buffers-re "^\\*") ; Ignore special buffers

(provide 'config-buffers)
;;; config-buffers.el ends here
