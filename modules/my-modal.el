;;; my-modal.el --- A robust modal editing system for Emacs

;;; Commentary:
;; A modal editing system for Emacs with normal, insert, visual, yank, and delete modes

;;; Code:


(require 'custom-functions)

;; Prevent warnings about undefined variables/functions
(defvar my-modal-mode)

;; ================================== Custom Faces ===================================
(defgroup my-modal nil
  "Custom modal editing system for Emacs."
  :group 'editing)

(defface my-modal-normal-cursor
  '((t (:background "#50fa7b" :foreground "black")))
  "Face for normal mode cursor."
  :group 'my-modal)

(defface my-modal-insert-cursor
  '((t (:background "#61afef" :foreground "black")))
  "Face for insert mode cursor."
  :group 'my-modal)

(defface my-modal-visual-cursor
  '((t (:background "#ff79c6" :foreground "black")))
  "Face for visual mode cursor."
  :group 'my-modal)

(defface my-modal-yank-cursor
  '((t (:background "#bd93f9" :foreground "black")))
  "Face for yank mode cursor."
  :group 'my-modal)

(defface my-modal-delete-cursor
  '((t (:background "#ff5555" :foreground "black")))
  "Face for delete mode cursor."
  :group 'my-modal)

;; =================================== State Management =============================
(defvar my-modal-state-change-hook nil
  "Hook run after modal state changes.")
  
(defvar my-modal-state nil
  "Current state of the modal system.")

(defvar-local my-modal-cursor-type 'box
  "Buffer-local cursor type.")

;; Define buffers where we want insert mode
(defvar my-modal-insert-buffers
  '("*shell*" "*terminal*" "*eshell*" "*vterm*")
  "List of buffer names where we want insert mode.")

(defun my-modal-should-use-insert-p ()
  "Check if current buffer should use insert mode."
  (or (minibufferp)
      (member (buffer-name) my-modal-insert-buffers)))

;; ==================================== Keymaps ===================================
(defvar my-modal-normal-map (make-sparse-keymap)
  "Keymap for normal mode.")

(defvar my-modal-insert-map (make-sparse-keymap)
  "Keymap for insert mode.")

(defvar my-modal-visual-map (make-sparse-keymap)
  "Keymap for visual mode.")

(defvar my-modal-yank-map (make-sparse-keymap)
  "Keymap for yank mode.")

(defvar my-modal-delete-map (make-sparse-keymap)
  "Keymap for delete mode.")

(defvar my-modal-menu1-map (make-sparse-keymap)
  "Keymap for menu1.")

;; ================================= Cursor Management ============================
(defun my-modal-set-cursor (type color)
  "Set cursor TYPE and COLOR globally and locally."
  (setq-local cursor-type type)
  (setq cursor-type type)
  (when color
    (set-cursor-color color))
  (when (not (display-graphic-p))
    (send-string-to-terminal 
     (if (eq type 'box) "\e[1 q" "\e[5 q"))))

;; ================================= State Transitions ===========================
(defun my-modal-check-visual-state ()
  "Check if visual state should be maintained."
  (when (and (eq my-modal-state 'visual)
             (not (region-active-p)))
    (my-modal-enter-insert-mode)))

(defun my-modal-enter-normal-mode ()
  "Enter normal mode."
  (interactive)
  (remove-hook 'post-command-hook #'my-modal-check-visual-state t)
  (setq my-modal-state 'normal)
  (unless (my-modal-should-use-insert-p)
    (my-modal-set-cursor 'box (face-background 'my-modal-normal-cursor))
    (use-local-map my-modal-normal-map))
  (when (region-active-p)
    (deactivate-mark))
  (run-hooks 'my-modal-state-change-hook)  ; Add this line
  (force-mode-line-update t))              ; Add this line too

(defun my-modal-enter-insert-mode ()
  "Enter insert mode."
  (interactive)
  (remove-hook 'post-command-hook #'my-modal-check-visual-state t)
  (setq my-modal-state 'insert)
  (my-modal-set-cursor 'bar (face-background 'my-modal-insert-cursor))
  (unless (my-modal-should-use-insert-p)
    (use-local-map my-modal-insert-map)
   (run-hooks 'my-modal-state-change-hook)  ; Add this line
   (force-mode-line-update t)))

(defun my-modal-enter-visual-mode ()
  "Enter visual mode."
  (interactive)
  (setq my-modal-state 'visual)
  (my-modal-set-cursor 'box (face-background 'my-modal-visual-cursor))
  (use-local-map my-modal-visual-map)
  (unless (region-active-p)
    (set-mark-command nil))
  (add-hook 'post-command-hook #'my-modal-check-visual-state nil t)
  (run-hooks 'my-modal-state-change-hook)  ; Add this line
  (force-mode-line-update t))

(defun my-modal-enter-yank-mode ()
  "Enter yank mode."
  (interactive)
  (setq my-modal-state 'yank)
  (my-modal-set-cursor 'box (face-background 'my-modal-yank-cursor))
  (use-local-map my-modal-yank-map)
  (run-hooks 'my-modal-state-change-hook)  ; Add this line
  (force-mode-line-update t))

(defun my-modal-enter-delete-mode ()
  "Enter delete mode."
  (interactive)
  (setq my-modal-state 'delete)
  (my-modal-set-cursor 'box (face-background 'my-modal-delete-cursor))
  (use-local-map my-modal-delete-map)
  (run-hooks 'my-modal-state-change-hook)
  (force-mode-line-update t))


(defun my-modal-enter-menu1-mode ()
  "Enter delete mode."
  (interactive)
  (setq my-modal-state 'menu1)
  ;; (my-modal-set-cursor 'box (face-background 'my-modal-normal-cursor))
  (use-local-map my-modal-menu1-map)
  (run-hooks 'my-modal-state-change-hook)
  (force-mode-line-update t))

;;===================== initialization========================  

;; Handle both new buffers and initial scratch buffer
(add-hook 'emacs-startup-hook (lambda () 
                               (with-current-buffer "*scratch*"
                                 (my-modal-enter-normal-mode))))
                                 
                                 
(defun my-modal-setup-for-buffer ()
  "Setup modal state only for new buffers, not during typing."
  (unless (minibufferp)
    (when (and (not (eq major-mode 'fundamental-mode))
               (not (eq major-mode 'minibuffer-mode))
               (not (derived-mode-p 'special-mode)))
      (my-modal-enter-normal-mode))))

;; Replace your current hook with this
(add-hook 'after-change-major-mode-hook #'my-modal-setup-for-buffer)

;;========================== hydra ===========================

(defhydra hydra-change-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
  	      (my-corfu-quit)
	      (my-modal-enter-normal-mode))))
   


(provide 'my-modal)
;;; my-modal.el ends here
