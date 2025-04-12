;;; config-snippets.el --- Simple Tempel configuration with pretty-hydra -*- lexical-binding: t -*-

;;; Commentary:
;; Simple snippet functionality using pretty-hydra and tempel

;;; Code:

(require 'hydra)
(require 'pretty-hydra)
(require 'tempel)

;; ========================= Helper Functions ============================

;; Define template directory
(defvar tempel-template-dir (expand-file-name "templates" user-emacs-directory)
  "Directory for Tempel template files.")

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

(defun tempel-edit ()
  "Edit a template file."
  (interactive)
  (let* ((files (directory-files tempel-template-dir nil "\\.eld$"))
         (file (completing-read "Edit template file: " files nil t)))
    (find-file (expand-file-name file tempel-template-dir))))

(defun tempel-save ()
  "Save all template buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (buffer-file-name)
                 (string-match "\\.eld$" (buffer-file-name))
                 (buffer-modified-p))
        (save-buffer))))
  (message "All template buffers saved."))

(defun tempel-refresh ()
  "Refresh templates and show message."
  (interactive)
  (tempel-reload)
  (message "Templates refreshed successfully."))

;; ========================= Tempel ================================
;; Tempel configuration to work with Corfu

(use-package tempel
  :ensure t
  :bind (("M-[" . tempel-next)
	 ("M-]" . tempel-previous))
  :custom
  ;; Use your templates directory
  (tempel-path (let ((template-dir (expand-file-name "templates" user-emacs-directory)))
                 (when (file-directory-p template-dir)
                   (directory-files template-dir t "\\.eld$"))))
  :config
  ;; Setup completion-at-point integration
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  
  ;; Add to various mode hooks
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'web-mode-hook 'tempel-setup-capf)
  
  ;; Enable global abbreviation mode
  (global-tempel-abbrev-mode))


;; ========================= Snippets Pretty-Hydra ================================

(pretty-hydra-define hydra-snippets
  (:title "Tempel Snippets" :quit-key "q" :color blue)
  (
   "Basic"
   (
    ("i" tempel-insert "Insert")
    ("f" tempel-next "Forward")
    ("p" tempel-previous "Previous")
    )
   
   "Templates"
   (("n" tempel-new "New")
    ("e" tempel-edit "Edit")
    ("l" tempel-list "List")
    ("v" tempel-view "View"))
   
   "Actions"
   (("r" tempel-reload "Reload")
    ("d" (lambda () 
           (interactive)
           (find-file tempel-template-dir)) "Open Dir")
    ("s" tempel-save "Save All")
    ("R" tempel-refresh "Refresh"))))


(provide 'config-snippets)
;;; config-snippets.el ends here
