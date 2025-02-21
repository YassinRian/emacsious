;; config-python-company.el - Python development configuration

(require 'pyenv-mode)
(require 'pyvenv)
(require 'lsp-mode)
(require 'lsp-pyright)
(require 'lsp-ui)
(require 'python-black)
(require 'python-pytest)
(require 'projectile)

;; Set up pyenv path
(let ((pyenv-path (expand-file-name "~/.pyenv")))
  (setenv "PYENV_ROOT" pyenv-path)
  (setenv "PATH" (concat pyenv-path "/shims:" (getenv "PATH"))))

;; Enable pyenv and pyvenv
(pyenv-mode 1)
(pyvenv-mode 1)

;; Automatically activate virtualenv for projects
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

;; LSP and Pyright configuration
(defun my/set-pyright-python-path ()
  "Set the Python path for pyright based on current pyenv version."
  (let ((python-exec-path (string-trim (shell-command-to-string "pyenv which python"))))
    (setq-local lsp-pyright-python-executable-cmd python-exec-path)))

;; Configure LSP for Python
(setq lsp-pyright-multi-root nil
      lsp-pyright-auto-import-completions t
      lsp-pyright-use-library-code-for-types t
      lsp-pyright-typechecking-mode "basic")

;; Company configuration for Python
(defun my/setup-python-company ()
  "Set up company for Python development."
  (company-mode +1)
  (company-box-mode 1)
  (setq-local company-backends
              '((company-files
                 company-capf
                 company-keywords
                 company-yasnippet)
                (company-dabbrev-code company-dabbrev)))
  (setq-local company-idle-delay 0.1
              company-minimum-prefix-length 1
              lsp-idle-delay 0.1
              gc-cons-threshold (* 100 1024 1024)
              read-process-output-max (* 1024 1024)))

;; Python mode hooks
(add-hook 'python-mode-hook #'my/setup-python-company)
(add-hook 'python-mode-hook #'my/set-pyright-python-path)
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'projectile-after-switch-project-hook #'my/pyenv-activate-for-project)
(add-hook 'python-mode-hook #'my/pyenv-activate-for-project)

;; Format with black on save (optional)
(add-hook 'python-mode-hook #'python-black-on-save-mode)

;; Python testing configuration
(setq python-pytest-executable "pytest"
      python-pytest-arguments '("--color" "--verbose"))

(provide 'config-python-company)
