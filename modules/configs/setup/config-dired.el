;;; config-dired.el --- Advanced dired configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive dired functionality using Hydra

;;; Code:

(require 'hydra)
(require 'dired)
(require 'dired-x)
(require 'dired-aux)

;; ========================= Dired Hydra ================================

(defhydra hydra-dired (:hint nil :exit t)
  "
  Dired Commands
  ^Navigation^        ^Mark^             ^Actions^          ^View^
  ^^^^^^^^-----------------------------------------------------------------
  _n_: Next          _m_: Mark          _R_: Rename        _v_: View
  _p_: Previous      _u_: Unmark        _C_: Copy          _V_: View Other
  _j_: Jump          _t_: Toggle        _D_: Delete        _o_: Sort
  _f_: Find          _*_: Executables   _Z_: Compress      _(_: Details
  
  ^Mark by^          ^Filter^           ^Operations^       ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _._: Extension     _/_: Filter        _T_: Touch         _W_: Wdired
  _@_: Symlinks      _l_: Filter List   _O_: Chmod         _+_: Create Dir
  _#_: Auto Save     _g_: Refresh       _G_: Chgrp         _!_: Shell Cmd
  _%_: Regexp        _h_: Hide Dots     _M_: Chmod         _&_: Async Cmd
  "
  ;; Navigation
  ("n" dired-next-line "next")
  ("p" dired-previous-line "previous")
  ("j" dired-goto-file "jump to file")
  ("f" dired-find-file "find file")
  
  ;; Basic Marking
  ("m" dired-mark "mark")
  ("u" dired-unmark "unmark")
  ("t" dired-toggle-marks "toggle marks")
  ("*" dired-mark-executables "mark executables")
  
  ;; Mark by Type
  ("." dired-mark-extension "mark extension")
  ("@" dired-mark-symlinks "mark symlinks")
  ("#" dired-mark-auto-save-files "mark autosave")
  ("%" dired-mark-files-regexp "mark regexp")
  
  ;; Filtering
  ("/" dired-narrow "filter")
  ("l" dired-filter-group-mode "filter list")
  ("g" revert-buffer "refresh")
  ("h" dired-hide-dotfiles-mode "hide dotfiles")
  
  ;; Actions
  ("R" dired-do-rename "rename")
  ("C" dired-do-copy "copy")
  ("D" dired-do-delete "delete")
  ("Z" dired-do-compress "compress")
  
  ;; Operations
  ("T" dired-do-touch "touch")
  ("O" dired-do-chmod "chmod")
  ("G" dired-do-chgrp "chgrp")
  ("M" dired-do-chmod "chmod")
  
  ;; View
  ("v" dired-view-file "view")
  ("V" dired-view-file-other-window "view other")
  ("o" dired-sort-toggle-or-edit "sort")
  ("(" dired-hide-details-mode "details")
  
  ;; Special
  ("W" wdired-change-to-wdired-mode "wdired")
  ("+" dired-create-directory "create dir")
  ("!" dired-do-shell-command "shell command")
  ("&" dired-do-async-shell-command "async command")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c d") #'hydra-dired/body)

;; ========================= Helper Functions ============================

(defun dired-hide-dotfiles-mode ()
  "Toggle hiding of dotfiles in dired."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (progn
        (dired-mark-files-regexp "^\\.")
        (dired-do-kill-lines))
    (message "Not in a dired buffer")))

(defun dired-sort-size ()
  "Sort dired buffer by size."
  (interactive)
  (dired-sort-other "-laSr"))

(defun dired-sort-extension ()
  "Sort dired buffer by extension."
  (interactive)
  (dired-sort-other "-laX"))

(defun dired-sort-ctime ()
  "Sort dired buffer by create time."
  (interactive)
  (dired-sort-other "-lact"))

;; ========================= Dired Configuration =======================

;; Basic dired settings
(setq dired-listing-switches "-alh"                  ; Human-readable sizes
      dired-dwim-target t                           ; Guess target directory
      dired-recursive-copies 'always                ; Always copy recursively
      dired-recursive-deletes 'always              ; Always delete recursively
      delete-by-moving-to-trash t                  ; Move to trash instead of delete
      dired-auto-revert-buffer t)                  ; Auto revert buffer

;; Dired-x configuration
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))     ; Omit dot files

;; Enable some advanced features
(put 'dired-find-alternate-file 'disabled nil)     ; Enable 'a' keybinding
(setq dired-guess-shell-alist-user                 ; File associations
      '(("\\.pdf\\'" "evince")
        ("\\.docx\\'" "libreoffice")
        ("\\.xlsx\\'" "libreoffice")
        ("\\.png\\'" "feh")
        ("\\.jpg\\'" "feh")))

;; Enable auxiliary features
(add-hook 'dired-mode-hook 'dired-hide-details-mode)  ; Start in hide-details mode
(add-hook 'dired-mode-hook 'hl-line-mode)            ; Highlight current line
(add-hook 'dired-mode-hook 'dired-omit-mode)         ; Enable omit mode

;; WDired configuration
(setq wdired-allow-to-change-permissions t         ; Allow permission editing
      wdired-create-parent-directories t)          ; Create parent dirs as needed

(provide 'config-dired)
;;; config-dired.el ends here
