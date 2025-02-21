;;; config-help.el --- Advanced help system configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive help functionality using Hydra
;; Integrates help, info, apropos, and describe functions

;;; Code:

(require 'hydra)
(require 'help)
(require 'helpful)
(require 'info)
(require 'apropos)

;; ========================= Help Hydra ================================

(defhydra hydra-help (:hint nil :exit t)
  "
  Help Commands
  ^Basic^            ^Describe^         ^Info^             ^Find^
  ^^^^^^^^-----------------------------------------------------------------
  _h_: Help         _f_: Function      _i_: Info          _l_: Library
  _k_: Key          _v_: Variable      _m_: Manual        _F_: Function
  _w_: Where        _s_: Symbol        _n_: Node          _C_: Command
  _a_: Apropos      _p_: Package       _I_: Index         _V_: Variable

  ^Documentation^    ^Modes^            ^Package^          ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _d_: Docs at Pt   _M_: Major Mode    _P_: List Pkgs     _t_: Tutorial
  _D_: Dir          _e_: Mode Enabled   _L_: Load Path     _c_: Coding
  _b_: Bindings     _E_: Mode All      _S_: Sys Info      _W_: Woman
  _r_: Recent       _x_: Extra Mode    _u_: Update Docs   _A_: About
  "
  ;; Basic Help
  ("h" help-for-help "help system")
  ("k" helpful-key "key help")
  ("w" where-is "where is")
  ("a" apropos "apropos")
  
  ;; Describe Objects
  ("f" helpful-callable "describe function")
  ("v" helpful-variable "describe variable")
  ("s" helpful-symbol "describe symbol")
  ("p" describe-package "describe package")
  
  ;; Info Navigation
  ("i" info "info")
  ("m" info-display-manual "manual")
  ("n" info-lookup-symbol "lookup symbol")
  ("I" info-apropos "info apropos")
  
  ;; Find Things
  ("l" find-library "find library")
  ("F" find-function "find function")
  ("C" find-command "find command")
  ("V" find-variable "find variable")
  
  ;; Documentation
  ("d" helpful-at-point "doc at point")
  ("D" list-directory "directory")
  ("b" describe-bindings "key bindings")
  ("r" view-lossage "recent keys")
  
  ;; Modes
  ("M" describe-mode "major mode")
  ("e" describe-enabled-modes "enabled modes")
  ("E" describe-all-modes "all modes")
  ("x" describe-minor-mode "minor mode")
  
  ;; Package Management
  ("P" list-packages "list packages")
  ("L" describe-load-path "load path")
  ("S" describe-system "system info")
  ("u" help-update-directory "update docs")
  
  ;; Special
  ("t" help-with-tutorial "tutorial")
  ("c" describe-coding-system "coding system")
  ("W" woman "man pages")
  ("A" about-emacs "about emacs")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c h") #'hydra-help/body)

;; ========================= Helper Functions ============================

(defun describe-enabled-modes ()
  "Display all enabled minor modes in current buffer."
  (interactive)
  (let ((modes ""))
    (mapc (lambda (mode)
            (when (and (symbolp mode)
                      (boundp mode)
                      (symbol-value mode))
              (setq modes (concat modes
                                (format "- %s\n" mode)))))
          minor-mode-list)
    (with-help-window "*Enabled Modes*"
      (princ "Enabled minor modes:\n\n")
      (princ modes))))

(defun describe-all-modes ()
  "Display all available major and minor modes."
  (interactive)
  (apropos-command "mode$" nil 
                   (lambda (symbol)
                     (string-match "-mode$"
                                 (symbol-name symbol)))))

(defun find-command (command)
  "Find the source of COMMAND."
  (interactive
   (list (intern-soft
          (completing-read "Find command: "
                          obarray 'commandp t))))
  (find-function command))

(defun describe-system ()
  "Display system information."
  (interactive)
  (with-help-window "*System Information*"
    (princ (format "System Info:
Emacs Version: %s
System Type: %s
Window System: %s
System Configuration: %s
Operating System: %s
Site Directory: %s"
                   emacs-version
                   system-type
                   window-system
                   system-configuration
                   system-name
                   data-directory))))

;; ========================= Help Configuration =======================

;; Help window configuration
(setq help-window-select t                     ; Select help window
      help-window-keep-selected t              ; Keep help window selected
      help-enable-variable-value-editing t)    ; Enable value editing

;; Info configuration
(setq Info-additional-directory-list '("~/.emacs.d/info")
      Info-directory-list '("~/.emacs.d/info"
                           "/usr/local/share/info"
                           "/usr/share/info"))

;; Helpful configuration
(setq helpful-max-buffers 5                    ; Maximum helpful buffers
      helpful-switch-buffer-function           ; Switch buffer function
      #'pop-to-buffer)

;; Apropos configuration
(setq apropos-do-all t                        ; Search more extensively
      apropos-sort-by-scores t)               ; Sort by relevance

;; Add extra documentation directories
(dolist (dir '("~/.emacs.d/docs"
               "~/.emacs.d/manuals"))
  (when (file-directory-p dir)
    (add-to-list 'Info-directory-list dir)))

;; Enable helpful mode
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
(global-set-key (kbd "C-h o") #'helpful-symbol)

(provide 'config-help)
;;; config-help.el ends here
