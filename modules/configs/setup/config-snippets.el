;;; config-snippets.el --- Snippet configuration using tempel -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive snippet functionality using Hydra and tempel

;;; Code:

(require 'hydra)
(require 'tempel)

;; ========================= Snippets Hydra ================================

(defhydra hydra-snippets (:hint nil :exit t)
  "
  Snippet Commands
  ^Basic^            ^Templates^        ^History^          ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _i_: Insert       _n_: New           _p_: Previous      _r_: Reload
  _c_: Complete     _e_: Edit          _h_: History       _d_: Directory 
  _a_: Abort        _l_: List          _f_: Forward       _s_: Save All
  _t_: Toggle       _v_: View          _b_: Backward      _R_: Refresh
  "
  ;; Basic Operations
  ("i" tempel-insert "insert")
  ("c" tempel-complete "complete")
  ("a" tempel-abort "abort")
  ("t" tempel-toggle "toggle")
  
  ;; Template Management
  ("n" tempel-new "new template")
  ("e" tempel-edit "edit template")
  ("l" tempel-list "list templates")
  ("v" tempel-view "view template")
  
  ;; History Navigation
  ("p" tempel-previous "previous")
  ("h" tempel-history "history")
  ("f" tempel-forward "forward")
  ("b" tempel-backward "backward")
  
  ;; Special Operations
  ("r" tempel-reload "reload")
  ("d" (lambda () 
         (interactive)
         (find-file tempel-template-dir)) "open directory")
  ("s" tempel-save "save all")
  ("R" (lambda ()
         (interactive)
         (tempel-reload)
         (message "Templates refreshed")) "refresh templates")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c n") #'hydra-snippets/body)

;; ========================= Helper Functions ============================

(defun tempel-new ()
  "Create a new template."
  (interactive)
  (let* ((name (read-string "Template name: "))
         (file (expand-file-name 
                (format "%s.eld" name)
                tempel-template-dir)))
    (find-file file)
    (insert ";; -*- mode: lisp-data -*-\n\n")))

(defun tempel-view ()
  "View available templates for current major mode."
  (interactive)
  (let ((templates (tempel--templates)))
    (with-current-buffer (get-buffer-create "*Tempel Templates*")
      (erase-buffer)
      (dolist (template templates)
        (insert (format "Template: %s\n" (car template))
                (format "Expansion: %s\n\n" (cdr template))))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

(defun tempel-list ()
  "List all template files."
  (interactive)
  (let ((files (directory-files tempel-template-dir nil "\\.eld$")))
    (with-current-buffer (get-buffer-create "*Tempel Files*")
      (erase-buffer)
      (dolist (file files)
        (insert (format "%s\n" file)))
      (goto-char (point-min))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;; ========================= Tempel Configuration =======================

;; Basic template settings
(setq tempel-template-dir (expand-file-name "templates" user-emacs-directory))

;; Create template directory if it doesn't exist
(unless (file-exists-p tempel-template-dir)
  (make-directory tempel-template-dir t))

;; Sample templates for different modes
(setq tempel-path
      (list
       (expand-file-name "templates/*" user-emacs-directory)))

;; Default templates
(with-temp-buffer
  (let ((default-template-file
          (expand-file-name "default.eld" tempel-template-dir)))
    (unless (file-exists-p default-template-file)
      (insert ";; -*- mode: lisp-data -*-

fundamental-mode

(today (format-time-string \"%Y-%m-%d\"))
(time (format-time-string \"%H:%M:%S\"))
(uuid (org-id-uuid))

prog-mode

(fixme \"FIXME: \")
(todo \"TODO: \")
(bug \"BUG: \")
(hack \"HACK: \")

emacs-lisp-mode

(autoload \";;;###autoload\")
(pt \"(point)\")
(lambda \"(lambda ()\n  )\")
(int \"(interactive)\")
(defun \"(defun \")
(let \"(let (())\n  )\")
(while \"(while \")
")
      (write-file default-template-file))))

;; Enable completion at point
(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches.
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

;; Hooks
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

;; Completion style settings
(setq tempel-trigger-prefix "<"
      tempel-path (list (expand-file-name "templates/" user-emacs-directory))
      tempel-include-file-name t)

(provide 'config-snippets)
;;; config-snippets.el ends here
