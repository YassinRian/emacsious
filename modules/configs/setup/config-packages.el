;;; config-bookmarks.el --- Advanced bookmark configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive bookmark functionality using Hydra

;;; Code:

;; ========================
;; Flycheck (linter)

(use-package flycheck
  :config
  (global-flycheck-mode)

   ;; Optional: Django project detection and auto-configuration
   (defun my/setup-django-flake8 ()
     "Set up flake8 for Django if in a Django project."
     (when (and (buffer-file-name)
		(locate-dominating-file (buffer-file-name) "manage.py"))
       (setq-local flycheck-flake8-maximum-line-length 100)
       (setq-local flycheck-flake8-maximum-complexity 10)
       ;; Add common Django files to exclude
       (setq-local flycheck-flake8-excluded-files
		   (append flycheck-flake8-excluded-files
			   '("migrations" "settings.py")))))

   (add-hook 'python-mode-hook #'my/setup-django-flake8)

  )

;;---------//-----------------
;; ===========================
;; Flycheck

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (global-flycheck-mode))

;;---------//-----------------


(provide 'config-packages)
