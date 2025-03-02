;;; special-buffers.el --- Navigation system for special buffers using direct keybindings

;;; Commentary:
;; This package provides a consistent navigation system across Emacs special buffers
;; using direct local-set-key bindings for maximum reliability.

;;; Code:

(require 'cl-lib)

;; Configuration variables
(defvar special-buffer-nav-key-up "i"
  "Key for moving up in special buffer navigation.")

(defvar special-buffer-nav-key-down "o"
  "Key for moving down in special buffer navigation.")

(defvar special-buffer-nav-key-forward ";"
  "Key for moving forward in special buffer navigation.")

(defvar special-buffer-nav-key-backward "j"
  "Key for moving backward in special buffer navigation.")

;; Buffer-specific customization registry
(defvar special-buffer-nav-alist
  '((ibuffer-mode
     :predicate (lambda () (eq major-mode 'ibuffer-mode))
     :up-command previous-line
     :down-command next-line 
     :forward-command forward-word
     :backward-command backward-word
     :action ibuffer-visit-buffer)
    (dired-mode
     :predicate (lambda () (eq major-mode 'dired-mode))
     :up-command dired-previous-line
     :down-command dired-next-line
     :forward-command dired-next-dirline
     :backward-command dired-prev-dirline
     :action dired-find-file)
    (Buffer-menu-mode
     :predicate (lambda () (eq major-mode 'Buffer-menu-mode))
     :up-command previous-line
     :down-command next-line
     :forward-command forward-word
     :backward-command backward-word
     :action Buffer-menu-this-window)
    (package-menu-mode
     :predicate (lambda () (eq major-mode 'package-menu-mode))
     :up-command previous-line
     :down-command next-line
     :forward-command forward-word
     :backward-command backward-word
     :action package-menu-describe-package)
    (messages-buffer
     :predicate (lambda () (string= (buffer-name) "*Messages*"))
     :up-command previous-line
     :down-command next-line
     :forward-command forward-word
     :backward-command backward-word
     :action (lambda () (message "Current line: %s" (thing-at-point 'line t)))))
  "Registry of special buffer types with custom navigation commands.")

;; Core functionality
(defun special-buffer-p ()
  "Return t if the current buffer is a special buffer that supports navigation."
  (cl-some (lambda (item) 
             (funcall (plist-get (cdr item) :predicate)))
           special-buffer-nav-alist))

(defun get-special-buffer-config ()
  "Get the navigation configuration for the current special buffer."
  (cdr (cl-find-if (lambda (item)
                     (funcall (plist-get (cdr item) :predicate)))
                   special-buffer-nav-alist)))

(defun setup-special-buffer-keys ()
  "Set up the navigation keys for the current special buffer."
  (when-let* ((config (get-special-buffer-config)))
    ;; For Messages buffer, make it read-only
    (when (string= (buffer-name) "*Messages*")
      (setq buffer-read-only t))
    
    ;; Use local-set-key for the buffer-specific commands
    (local-set-key (kbd special-buffer-nav-key-up)
                  (plist-get config :up-command))
    (local-set-key (kbd special-buffer-nav-key-down)
                  (plist-get config :down-command))
    (local-set-key (kbd special-buffer-nav-key-forward)
                  (plist-get config :forward-command))
    (local-set-key (kbd special-buffer-nav-key-backward)
                  (plist-get config :backward-command))
    
    ;; Common buffer movement commands
    (local-set-key (kbd "<") #'beginning-of-buffer)
    (local-set-key (kbd ">") #'end-of-buffer)
    (local-set-key (kbd "q") #'quit-window)
    
    ;; Selection action
    (local-set-key (kbd "RET") 
                  (let ((action (plist-get config :action)))
                    (if (functionp action)
                        action
                      (lambda () (interactive) (call-interactively action)))))
    
    (message "Special buffer navigation keys set up for %s" (buffer-name))))

;; Buffer-specific utility function
(defun messages-buffer-nav ()
  "Switch to the *Messages* buffer and set up navigation."
  (interactive)
  (switch-to-buffer "*Messages*")
  (goto-char (point-max))
  (setup-special-buffer-keys))

;; Add a new buffer type to the navigation system
(defun special-buffer-nav-register (buffer-type &rest config)
  "Register BUFFER-TYPE with CONFIG in the special buffer navigation system."
  (setq special-buffer-nav-alist 
        (cons (cons buffer-type config) 
              (assq-delete-all buffer-type special-buffer-nav-alist))))

;; Install hooks
(defun special-buffer-nav-setup ()
  "Set up special buffer navigation hooks."
  (interactive)
  
  ;; Standard special mode hooks
  (dolist (hook '(ibuffer-mode-hook
                  dired-mode-hook
                  Buffer-menu-mode-hook
                  package-menu-mode-hook
                  special-mode-hook))
    (add-hook hook #'setup-special-buffer-keys))
  
  ;; For the Messages buffer
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (and (string= (buffer-name) "*Messages*")
                         (not (local-key-binding (kbd "i"))))
                (setup-special-buffer-keys)))))

;; Initialize
(special-buffer-nav-setup)

(provide 'special-buffers)
;;; special-buffers.el ends here
