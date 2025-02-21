;;; config-terminal.el --- Advanced terminal configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive terminal functionality using Hydra

;;; Code:

(require 'hydra)
(require 'vterm)
(require 'eshell)
(require 'term)
(require 'ansi-term)

;; ========================= Terminal Hydra ================================

(defhydra hydra-terminal (:hint nil :exit t)
  "
  Terminal Commands
  ^Launch^           ^VTerm^            ^Eshell^           ^Shell^
  ^^^^^^^^-----------------------------------------------------------------
  _v_: VTerm        _c_: Copy          _e_: Eshell        _s_: Shell
  _V_: VTerm Other  _p_: Paste         _E_: New Eshell    _S_: Shell Other
  _n_: New VTerm    _h_: History       _d_: Directory     _x_: Command
  _t_: Ansi Term    _r_: Reset         _C_: Clear         _T_: Toggle
  
  ^Buffer^           ^Navigate^         ^Send^             ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _k_: Kill         _b_: Backward      _i_: Input         _f_: Find File
  _R_: Rename       _f_: Forward       _l_: Line          _g_: Goto Dir
  _z_: Max          _j_: Jump          _y_: Yank          _m_: Multi Term
  _q_: Quit         _/_: Search        _w_: Word          _M_: Mode
  "
  ;; Launch Terminals
  ("v" vterm "vterm")
  ("V" vterm-other-window "vterm other")
  ("n" multi-vterm-next "new vterm")
  ("t" ansi-term "ansi term")
  
  ;; VTerm Operations
  ("c" vterm-copy-mode "copy")
  ("p" vterm-yank "paste")
  ("h" vterm-clear-scrollback "history")
  ("r" vterm-reset-cursor-point "reset")
  
  ;; Eshell Operations
  ("e" eshell "eshell")
  ("E" eshell-new "new eshell")
  ("d" eshell-toggle-cd "directory")
  ("C" eshell-clear-buffer "clear")
  
  ;; Shell Operations
  ("s" shell "shell")
  ("S" shell-other-window "shell other")
  ("x" shell-command "command")
  ("T" shell-toggle "toggle")
  
  ;; Buffer Management
  ("k" kill-buffer "kill")
  ("R" rename-buffer "rename")
  ("z" maximize-window "maximize")
  ("q" quit-window "quit")
  
  ;; Navigation
  ("b" previous-buffer "backward")
  ("f" next-buffer "forward")
  ("j" ace-jump-mode "jump")
  ("/" isearch-forward "search")
  
  ;; Send Input
  ("i" term-send-input "input")
  ("l" term-send-line "line")
  ("y" term-paste "yank")
  ("w" term-send-forward-word "word")
  
  ;; Special Operations
  ("f" find-file "find file")
  ("g" term-goto-dir "goto dir")
  ("m" multi-term "multi term")
  ("M" term-line-mode "mode")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c t") #'hydra-terminal/body)

;; ========================= Helper Functions ============================

(defun term-goto-dir ()
  "Change terminal directory to current buffer's directory."
  (interactive)
  (let ((dir default-directory))
    (when (derived-mode-p 'vterm-mode)
      (vterm-send-string (format "cd '%s'" dir)))
    (when (derived-mode-p 'term-mode)
      (term-send-string (format "cd '%s'\n" dir)))
    (when (derived-mode-p 'eshell-mode)
      (eshell/cd dir))))

(defun shell-toggle ()
  "Toggle between shell and previous buffer."
  (interactive)
  (if (string-match "\\*shell\\*" (buffer-name))
      (switch-to-prev-buffer)
    (shell)))

;; ========================= Terminal Configuration =====================

;; VTerm settings
(setq vterm-max-scrollback 10000
      vterm-buffer-name-string "vterm: %s"
      vterm-shell (executable-find "zsh"))

;; Set cursor style in vterm
(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local cursor-type 'bar)))

;; Term settings
(setq explicit-shell-file-name (executable-find "zsh")
      term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
      term-buffer-maximum-size 10000)

;; Eshell settings
(setq eshell-scroll-to-bottom-on-input t
      eshell-scroll-to-bottom-on-output t
      eshell-hist-ignoredups t
      eshell-history-size 1024
      eshell-buffer-maximum-lines 10000
      eshell-glob-case-insensitive t
      eshell-error-if-no-glob t
      eshell-prefer-lisp-functions nil)

;; Shell settings
(setq shell-file-name (executable-find "zsh")
      shell-command-switch "-c"
      shell-push-regexp "^[^#$%>]*[#$%>] *"
      shell-highlight-changes nil)

;; Comint settings (for shell mode)
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output t
      comint-scroll-show-maximum-output t
      comint-input-ignoredups t
      comint-input-ring-size 1000
      comint-prompt-read-only t)

;; Terminal faces
(custom-set-faces
 '(term-color-black ((t (:foreground "black" :background "black"))))
 '(term-color-red ((t (:foreground "red2" :background "red2"))))
 '(term-color-green ((t (:foreground "green2" :background "green2"))))
 '(term-color-yellow ((t (:foreground "yellow2" :background "yellow2"))))
 '(term-color-blue ((t (:foreground "blue2" :background "blue2"))))
 '(term-color-magenta ((t (:foreground "magenta2" :background "magenta2"))))
 '(term-color-cyan ((t (:foreground "cyan2" :background "cyan2"))))
 '(term-color-white ((t (:foreground "white" :background "white")))))

;; Terminal hooks
(add-hook 'term-mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil)
            (setq-local line-spacing 0)))

(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil)
            (setq-local line-spacing 0)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil)
            (setq-local line-spacing 0)))

;; Directory tracking
(add-hook 'term-mode-hook
          (lambda ()
            (term-set-escape-char ?\C-x)
            (make-local-variable 'dirtrack-list)
            (setq dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1))
            (dirtrack-mode 1)))

(provide 'config-terminal)
;;; config-terminal.el ends here
