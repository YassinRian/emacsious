;;; config-errors.el --- Advanced error handling configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive error navigation and management using Hydra
;; Integrates flycheck, flymake, and compilation errors

;;; Code:

(require 'hydra)
(require 'flycheck)
(require 'flymake)
(require 'compile)

;; ========================= Errors Hydra ================================

(defhydra hydra-errors (:hint nil :exit t)
  "
  Error Navigation
  ^Basic^            ^Flycheck^         ^Flymake^          ^Compile^
  ^^^^^^^^-----------------------------------------------------------------
  _n_: Next         _f_: First         _d_: Diagnostics   _c_: Compile
  _p_: Previous     _l_: List          _m_: Messages      _r_: Recompile
  _j_: Jump         _v_: Verify        _N_: Next Error    _k_: Kill
  _g_: Refresh      _b_: Buffer        _P_: Prev Error    _s_: Scroll
  
  ^Errors^           ^Display^          ^Actions^          ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _._: Check        _w_: Show          _x_: Explain       _t_: Toggle Mode
  _?_: Describe     _h_: Hide          _X_: Explain Echo  _T_: Theme
  _e_: Echo         _C_: Clear         _a_: Auto Check    _M_: Mode
  _i_: Info         _S_: Select        _z_: Syntax Only   _F_: Format
  "
  ;; Basic Navigation
  ("n" next-error "next error")
  ("p" previous-error "previous error")
  ("j" compilation-next-error "jump to error")
  ("g" (lambda () 
         (interactive)
         (flycheck-buffer)
         (flymake-start)) "refresh checks")
  
  ;; Flycheck Commands
  ("f" flycheck-first-error "first error")
  ("l" flycheck-list-errors "list errors")
  ("v" flycheck-verify-setup "verify setup")
  ("b" flycheck-buffer "check buffer")
  
  ;; Flymake Commands
  ("d" flymake-show-diagnostics-buffer "diagnostics")
  ("m" flymake-show-buffer-diagnostics "messages")
  ("N" flymake-goto-next-error "next flymake")
  ("P" flymake-goto-prev-error "prev flymake")
  
  ;; Compile Commands
  ("c" compile "compile")
  ("r" recompile "recompile")
  ("k" kill-compilation "kill compile")
  ("s" compilation-scroll-output "scroll output")
  
  ;; Error Management
  ("." flycheck-buffer "check now")
  ("?" flycheck-describe-checker "describe checker")
  ("e" flycheck-error-echo "echo error")
  ("i" flycheck-error-info "error info")
  
  ;; Display Options
  ("w" flycheck-display-error-at-point "show error")
  ("h" flycheck-error-hide "hide errors")
  ("C" flycheck-clear "clear errors")
  ("S" flycheck-select-checker "select checker")
  
  ;; Actions
  ("x" flycheck-explain-error-at-point "explain error")
  ("X" flycheck-explain-error-at-point-echo "explain echo")
  ("a" flycheck-toggle-check-syntax-automatically "toggle auto")
  ("z" flycheck-toggle-syntax-checking "toggle syntax")
  
  ;; Special
  ("t" flycheck-mode "toggle flycheck")
  ("T" flycheck-toggle-error-list "toggle theme")
  ("M" flymake-mode "toggle flymake")
  ("F" format-all-buffer "format buffer")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c e") #'hydra-errors/body)

;; ========================= Helper Functions ============================

(defun flycheck-toggle-error-list ()
  "Toggle the flycheck error list window."
  (interactive)
  (if (get-buffer-window flycheck-error-list-buffer)
      (delete-window (get-buffer-window flycheck-error-list-buffer))
    (flycheck-list-errors)))

(defun flycheck-error-info ()
  "Show detailed information about the current error."
  (interactive)
  (let ((err (flycheck-overlay-errors-at (point))))
    (if err
        (message "%s" (flycheck-error-message (car err)))
      (message "No error at point"))))

(defun format-all-buffer ()
  "Format the current buffer using appropriate formatter."
  (interactive)
  (cond
   ((eq major-mode 'python-mode)
    (python-black-buffer))
   ((eq major-mode 'rust-mode)
    (rust-format-buffer))
   ((eq major-mode 'js-mode)
    (prettier-js))
   (t
    (message "No formatter available for %s" major-mode))))

;; ========================= Error Configuration =======================

;; Flycheck configuration
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.5
      flycheck-display-errors-delay 0.2
      flycheck-indication-mode 'left-fringe
      flycheck-highlighting-mode 'symbols
      flycheck-standard-error-navigation t
      flycheck-emacs-lisp-load-path 'inherit)

;; Flymake configuration
(setq flymake-fringe-indicator-position 'left-fringe
      flymake-suppress-zero-counters t
      flymake-start-on-flymake-mode t
      flymake-no-changes-timeout 0.5
      flymake-start-on-save-buffer t)

;; Compilation configuration
(setq compilation-scroll-output 'first-error
      compilation-always-kill t
      compilation-skip-threshold 2
      compilation-auto-jump-to-first-error t)

;; Error navigation
(setq next-error-highlight t
      next-error-highlight-no-select t
      next-error-recenter t)

;; Error display
(setq flycheck-error-list-format
      [("File" 20)
       ("Line" 5 flycheck-error-list-entry-< :right-align t)
       ("Col" 3 nil :right-align t)
       ("Level" 8 flycheck-error-list-entry-level-<)
       ("ID" 6 t)
       ("Message" 0 t)])

;; Auto-enable error checking
(add-hook 'prog-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)

(provide 'config-errors)
;;; config-errors.el ends here
