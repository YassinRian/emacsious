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

;; JavaScript setup
(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning nil)
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'"))

;; JSON setup
(use-package json-mode
  :mode "\\.json\\'")

;; Web mode for HTML, CSS, and mixed content
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t))

;; Configure prettier for formatting
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("npx" "svelte-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((js-mode je typescript-mode typescript-ts-mode js-ts-mode)
                 . ("npx" "typescript-language-server" "--stdio"))))

(add-hook 'svelte-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'js2-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)


;; Install and configure emmet-mode for HTML, CSS, and Svelte
(use-package emmet-mode
  :hook ((html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (js-mode . emmet-mode)
         (web-mode . emmet-mode)
         (svelte-mode . emmet-mode))  ;; Enable emmet-mode for svelte-mode
  :config
  ;; Enable emmet quoting for attributes when needed
  (setq emmet-self-closing-tag-style " /")
  (setq emmet-preview-default t) ;; Enable preview by default

  ;; Add Emmet completions to Corfu
  (defun my/emmet-completions ()
    (when (and (bound-and-true-p emmet-mode)
               (or (derived-mode-p 'html-mode 'css-mode 'web-mode)
                   (member major-mode '(html-mode css-mode web-mode))))
      (let ((abbrev (emmet-expr-on-line)))
        (when abbrev
          (let ((completions (emmet-transform-suggestion abbrev)))
            (when completions
              (list :company-prefix abbrev
                    :company-candidates (list completions))))))))

  ;; Add to completion-at-point-functions
  (add-hook 'completion-at-point-functions #'my/emmet-completions 90 t)
  
  ;; Automatically activate emmet's jsx support when in js/svelte mode
  (add-hook 'svelte-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))
  (add-hook 'js-mode-hook (lambda () (setq emmet-expand-jsx-className? t))))



;; For Svelte mode specifically
(defun my-modal-svelte-reactivate ()
  "Reactivate modal mode in svelte regions."
  (when (eq my-modal-state 'normal)
    (my-modal-enter-normal-mode)))

(add-hook 'svelte-mode-hook (lambda ()
                             (add-hook 'after-change-major-mode-hook 
                                       'my-modal-svelte-reactivate nil t)))

;; ======================================= EOF =================================

(provide 'config-packages)
