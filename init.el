

;;========================================== Load Elpaca and install packages =================================

(load-file (expand-file-name "modules/my-packages.el" user-emacs-directory))
(load-file (expand-file-name "modules/my-modal.el" user-emacs-directory))
(load-file (expand-file-name "modules/my-keybindings.el" user-emacs-directory))
(load-file (expand-file-name "modules/my-modeline.el" user-emacs-directory))
(load-file (expand-file-name "modules/special-buffers.el" user-emacs-directory))

;; =========================================== General config =============================================

(setq visible-bell nil
      ring-bell-function #'ignore)

;; ====================   Store backup files in .emacs.d/backups

;; Ensure backup directory exists
(let ((backup-dir (expand-file-name "backups" user-emacs-directory)))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t)))

;; Set backup directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory))))
      
;; Store auto-save files in specific directory
(make-directory (expand-file-name "auto-saves/" user-emacs-directory) t)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; Make backups by copying
(setq backup-by-copying t)

;; Delete old versions
(setq delete-old-versions t)

;; Keep 5 newest versions
(setq kept-new-versions 5)

;; Keep 2 oldest versions
(setq kept-old-versions 2)

;; Use version numbers for backup files
(setq version-control t)

;; ctrlf search 
;;(ctrlf-mode +1) 
;; ============================================= Recent files ====================================

;; Enable recentf mode
(recentf-mode 1)

;; Set the maximum number of saved items
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 100)

;; Save the list every 5 minutes
(run-at-time nil 300 'recentf-save-list)

;; Exclude some files from the recent files list
(setq recentf-exclude '("^/tmp/" "^/ssh:" "\\.emacs\\.d/elpa/"))

;;============================================== // =====================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages '(urgrep)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:background "#3d4451" :extend t))))
 '(bm-fringe-face ((t (:background "#bd93f9" :foreground "#bd93f9"))))
 '(marginalia-date ((t (:foreground "#00a972"))))
 '(marginalia-file-modes ((t (:foreground "#70819f"))))
 '(marginalia-file-name ((t (:foreground "#afbcbf"))))
 '(marginalia-file-owner ((t (:foreground "#b0a0a0"))))
 '(marginalia-installed ((t (:foreground "#af8aff"))))
 '(marginalia-modified ((t (:foreground "#ef656a"))))
 '(marginalia-size ((t (:foreground "#029fff"))))
 '(region ((t (:background "#44475a" :extend t))))
 '(secondary-selection ((t (:background "#3e4a5d" :extend t))))
 '(vertico-current ((t (:background "#11141f" :foreground "#afbcbf" :extend t))))
 '(vhl/default-face ((t (:background "#454545" :foreground nil)))))
