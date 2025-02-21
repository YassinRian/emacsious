

;; Load Elpaca and install packages
(load-file (expand-file-name "modules/my-packages.el" user-emacs-directory))

;; Load Modal System
(load-file (expand-file-name "modules/my-modal.el" user-emacs-directory))

;; Load functions
(load-file (expand-file-name "modules/my-functions.el" user-emacs-directory))

;; Load Keybindings
(load-file (expand-file-name "modules/my-keybindings.el" user-emacs-directory))

;; Load Operators
(load-file (expand-file-name "modules/my-modeline.el" user-emacs-directory))

