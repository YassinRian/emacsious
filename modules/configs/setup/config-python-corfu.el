;; config-python.el - Complete Python development environment

(require 'eglot)
(require 'python)
(require 'pyenv-mode)
(require 'pyvenv)
(require 'python-black)
(require 'python-pytest)
(require 'orderless)
(require 'cape)

;; Set up pyenv path
(let ((pyenv-path (expand-file-name "~/.pyenv")))
  (setenv "PYENV_ROOT" pyenv-path)
  (setenv "PATH" (concat pyenv-path "/shims:" (getenv "PATH"))))

;; Enable pyenv and pyvenv
(pyenv-mode 1)
(pyvenv-mode 1)

;; Eglot configuration
(setq eglot-connect-timeout 60
      eglot-sync-connect nil
      eglot-autoshutdown t
      eglot-events-buffer-size 0
      eglot-stay-out-of '(flymake)
      ;; Python specific server settings
      eglot-workspace-configuration
      '((:pyright . ((useLibraryCodeForTypes . t)
                     (autoImportCompletions . t)))))

;; Register Python language server with Eglot
(add-to-list 'eglot-server-programs
             `(python-mode . ("pyright-langserver" "--stdio")))

;; Completion settings
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((eglot (styles orderless))))

;; Pyenv and virtualenv management
(defun my/get-python-path ()
  "Get Python path from current pyenv version."
  (string-trim (shell-command-to-string "pyenv which python")))

(defun my/pyenv-activate-for-project ()
  "Automatically activate pyenv virtualenv for current project."
  (when (projectile-project-p)
    (let* ((project-dir (projectile-project-root))
           (project-name (projectile-project-name))
           (pyenv-name (concat project-name "-venv")))
      ;; Check for .python-version file
      (if (file-exists-p (expand-file-name ".python-version" project-dir))
          (pyenv-mode-set (string-trim
                          (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name ".python-version" project-dir))
                            (buffer-string))))
        ;; If no .python-version file, try to find a matching virtualenv
        (when (member pyenv-name (pyenv-mode-versions))
          (pyenv-mode-set pyenv-name)))
      ;; Look for and activate virtualenv in standard locations
      (let ((venv-path (expand-file-name ".venv" project-dir)))
        (when (file-directory-p venv-path)
          (pyvenv-activate venv-path))))))

;; Python environment setup
(defun my/setup-python-env ()
  "Set up Python development environment."
  ;; Start Eglot if not already running
  (unless (eglot-managed-p)
    (eglot-ensure))

  ;; Configure Python interpreter
  (setq python-shell-interpreter (my/get-python-path))

  ;; Configure completion sources
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-file
                     #'cape-dabbrev)))

  ;; Configure Python indentation
  (setq python-indent-offset 4)

  ;; Enable useful minor modes
  (electric-pair-mode 1)
  (electric-indent-mode 1))

;; Python mode hooks
(add-hook 'python-mode-hook #'my/setup-python-env)
(add-hook 'python-mode-hook #'python-black-on-save-mode)
(add-hook 'projectile-after-switch-project-hook #'my/pyenv-activate-for-project)
(add-hook 'python-mode-hook #'my/pyenv-activate-for-project)

;; Testing configuration
(setq python-pytest-executable "pytest"
      python-pytest-arguments '("--color" "--verbose"))

;; Indentation settings
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

;; Key bindings for Eglot
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions))

;; Format on save with black
(setq python-black-command "black"
      python-black-extra-args '("--line-length" "88"))

(provide 'config-python-corfu)
