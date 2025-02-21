;;; config-registers.el --- Advanced register configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive register functionality using Hydra

;;; Code:

(require 'hydra)
(require 'register)

;; ========================= Registers Hydra ================================

(defhydra hydra-registers (:hint nil :exit t)
  "
  Register Commands
  ^Store^            ^Jump/Insert^      ^View^             ^Rectangle^
  ^^^^^^^^-----------------------------------------------------------------
  _p_: Point        _j_: Jump Point    _v_: View         _c_: Copy Rect
  _w_: Window       _i_: Insert        _l_: List         _k_: Kill Rect
  _r_: Region       _g_: Jump Window   _e_: Edit         _y_: Yank Rect
  _n_: Number       _t_: Insert Text   _s_: Search       _o_: Open Rect
  
  ^Special^          ^Bookmarks^        ^Macros^          ^Clear^
  ^^^^^^^^-----------------------------------------------------------------
  _+_: Increment    _b_: Bookmark      _m_: Record       _d_: Delete
  _f_: Frameset     _B_: Jump Book     _M_: Last Macro   _D_: Clear All
  _h_: Helm         _a_: Annotate      _x_: Execute      _z_: Reset Num
  _K_: Kill Ring    _L_: List Books    _X_: Apply All    _Z_: Zero Num
  "
  ;; Store
  ("p" point-to-register "store point")
  ("w" window-configuration-to-register "store window")
  ("r" copy-to-register "store region")
  ("n" number-to-register "store number")
  
  ;; Jump/Insert
  ("j" jump-to-register "jump to point")
  ("i" insert-register "insert content")
  ("g" frameset-to-register "jump to window")
  ("t" append-to-register "insert text")
  
  ;; View
  ("v" view-register "view register")
  ("l" list-registers "list registers")
  ("e" edit-register "edit register")
  ("s" (lambda () 
         (interactive)
         (helm-register)) "search registers")
  
  ;; Rectangle
  ("c" copy-rectangle-to-register "copy rectangle")
  ("k" kill-rectangle "kill rectangle")
  ("y" yank-rectangle "yank rectangle")
  ("o" open-rectangle "open rectangle")
  
  ;; Special
  ("+" increment-register "increment")
  ("f" frameset-to-register "frameset")
  ("h" helm-register "helm interface")
  ("K" (lambda () 
         (interactive)
         (append-to-register ?k (car kill-ring))) "store kill ring")
  
  ;; Bookmarks
  ("b" bookmark-set-register "set bookmark")
  ("B" bookmark-jump-register "jump bookmark")
  ("a" bookmark-edit-annotation-register "annotate")
  ("L" bookmark-list-register "list bookmarks")
  
  ;; Macros
  ("m" (lambda () 
         (interactive)
         (kmacro-to-register ?m)) "record macro")
  ("M" (lambda () 
         (interactive)
         (insert-kbd-macro ?m)) "insert last macro")
  ("x" (lambda () 
         (interactive)
         (execute-kbd-macro (get-register ?m))) "execute macro")
  ("X" apply-macro-to-region-lines "apply to region")
  
  ;; Clear
  ("d" (lambda () 
         (interactive)
         (let ((reg (read-char "Delete register: ")))
           (setq register-alist
                 (assq-delete-all reg register-alist)))) "delete register")
  ("D" (lambda () 
         (interactive)
         (setq register-alist nil)) "clear all")
  ("z" (lambda () 
         (interactive)
         (let ((reg (read-char "Reset number register: ")))
           (set-register reg 0))) "reset number")
  ("Z" (lambda () 
         (interactive)
         (dolist (r register-alist)
           (when (numberp (cdr r))
             (set-register (car r) 0)))) "zero all numbers")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c r") #'hydra-registers/body)

;; ========================= Helper Functions ============================

(defun bookmark-set-register (register)
  "Set a bookmark in REGISTER."
  (interactive "cSet bookmark in register: ")
  (bookmark-set (string register))
  (message "Bookmark set in register '%c'" register))

(defun bookmark-jump-register (register)
  "Jump to bookmark in REGISTER."
  (interactive "cJump to bookmark in register: ")
  (bookmark-jump (string register)))

(defun bookmark-edit-annotation-register (register)
  "Edit annotation for bookmark in REGISTER."
  (interactive "cEdit annotation for register: ")
  (bookmark-edit-annotation (string register)))

(defun bookmark-list-register ()
  "List bookmarks stored in registers."
  (interactive)
  (let ((bookmarks (cl-remove-if-not
                   (lambda (r)
                     (and (stringp (cdr r))
                          (bookmark-get-bookmark (cdr r) t)))
                   register-alist)))
    (with-current-buffer (get-buffer-create "*Register Bookmarks*")
      (erase-buffer)
      (dolist (b bookmarks)
        (insert (format "Register '%c': %s\n"
                       (car b)
                       (cdr b))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;; ========================= Register Configuration =====================

;; Basic register settings
(setq register-preview-delay 0.5              ; Preview delay
      register-preview-function               ; Custom preview function
      #'register-preview-default
      register-separator ?-                   ; Separator for appending
      register-alist-preserve nil)           ; Don't preserve empty registers

;; Register preview window settings
(setq register-preview-window-height 10      ; Preview window height
      register-preview-window-width 40)      ; Preview window width

;; Auto-save registers
(add-hook 'kill-emacs-hook
          (lambda ()
            (with-temp-file "~/.emacs.d/registers"
              (prin1 register-alist (current-buffer)))))

;; Load saved registers
(when (file-exists-p "~/.emacs.d/registers")
  (with-temp-buffer
    (insert-file-contents "~/.emacs.d/registers")
    (setq register-alist (read (current-buffer)))))

;; Initialize some useful registers
(set-register ?e '(file . "~/.emacs.d/init.el"))           ; init.el
(set-register ?z '(file . "~/.zshrc"))                     ; zshrc
(set-register ?d "~/Downloads/")                           ; Downloads path
(set-register ?t (format-time-string "%Y-%m-%d"))         ; Today's date

(provide 'config-registers)
;;; config-registers.el ends here
