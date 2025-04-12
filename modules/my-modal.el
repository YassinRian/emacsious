;;; my-modal.el --- A robust modal editing system for Emacs

;;; Commentary:
;; A modal editing system for Emacs with normal, insert, visual, c-mode, and delete modes

;;; Code:


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

(defface my-modal-c-cursor
  '((t (:background "#bd93f9" :foreground "black")))
  "Face for c-mode mode cursor."
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
  '("*shell*" "*terminal*" "*vterm*")
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

(defvar my-modal-c-map (make-sparse-keymap)
  "Keymap for c-mode mode.")

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

;; Fix for my-modal.el to ensure highest priority

;; Define a minor mode to hold our keymaps
(define-minor-mode my-modal-mode
  "Toggle my-modal mode."
  :lighter " Modal"
  :global t
  :group 'my-modal
  (if my-modal-mode
      (my-modal-enter-normal-mode)
    (setq my-modal-state nil)))

;; Modify the state transition functions to use minor-mode-overriding-map-alist
;; which has the highest priority in Emacs

(defun my-modal-enter-normal-mode ()
  "Enter normal mode."
  (interactive)
  (remove-hook 'post-command-hook #'my-modal-check-visual-state t)
  (setq my-modal-state 'normal)
  (unless (my-modal-should-use-insert-p)
    (my-modal-set-cursor 'box (face-background 'my-modal-normal-cursor))
    ;; Use highest priority keymap mechanism
    (setq minor-mode-overriding-map-alist 
          (cons (cons 'my-modal-mode my-modal-normal-map)
                (assq-delete-all 'my-modal-mode minor-mode-overriding-map-alist))))
  (when (region-active-p)
    (deactivate-mark))
  (run-hooks 'my-modal-state-change-hook)
  (force-mode-line-update t))

(defun my-modal-enter-insert-mode ()
  "Enter insert mode."
  (interactive)
  (remove-hook 'post-command-hook #'my-modal-check-visual-state t)
  (setq my-modal-state 'insert)
  (my-modal-set-cursor 'bar (face-background 'my-modal-insert-cursor))
  ;; Use highest priority keymap mechanism
  (setq minor-mode-overriding-map-alist 
        (cons (cons 'my-modal-mode my-modal-insert-map)
              (assq-delete-all 'my-modal-mode minor-mode-overriding-map-alist)))
  (run-hooks 'my-modal-state-change-hook)
  (force-mode-line-update t))

(defun my-modal-enter-visual-mode ()
  "Enter visual mode."
  (interactive)
  (setq my-modal-state 'visual)
  (my-modal-set-cursor 'box (face-background 'my-modal-visual-cursor))
  ;; Use highest priority keymap mechanism
  (setq minor-mode-overriding-map-alist 
        (cons (cons 'my-modal-mode my-modal-visual-map)
              (assq-delete-all 'my-modal-mode minor-mode-overriding-map-alist)))
  (unless (region-active-p)
    (set-mark-command nil))
  (add-hook 'post-command-hook #'my-modal-check-visual-state nil t)
  (run-hooks 'my-modal-state-change-hook)
  (force-mode-line-update t))

(defun my-modal-enter-c-mode ()
  "Enter c-mode mode."
  (interactive)
  (setq my-modal-state 'c-mode)
  (my-modal-set-cursor 'box (face-background 'my-modal-c-cursor))
  ;; Use highest priority keymap mechanism
  (setq minor-mode-overriding-map-alist 
        (cons (cons 'my-modal-mode my-modal-c-map)
              (assq-delete-all 'my-modal-mode minor-mode-overriding-map-alist)))
  (run-hooks 'my-modal-state-change-hook)
  (force-mode-line-update t))

(defun my-modal-enter-delete-mode ()
  "Enter delete mode."
  (interactive)
  (setq my-modal-state 'delete)
  (my-modal-set-cursor 'box (face-background 'my-modal-delete-cursor))
  ;; Use highest priority keymap mechanism
  (setq minor-mode-overriding-map-alist 
        (cons (cons 'my-modal-mode my-modal-delete-map)
              (assq-delete-all 'my-modal-mode minor-mode-overriding-map-alist)))
  (run-hooks 'my-modal-state-change-hook)
  (force-mode-line-update t))

(defun my-modal-enter-menu1-mode ()
  "Enter menu1 mode."
  (interactive)
  (setq my-modal-state 'menu1)
  ;; Use highest priority keymap mechanism
  (setq minor-mode-overriding-map-alist 
        (cons (cons 'my-modal-mode my-modal-menu1-map)
              (assq-delete-all 'my-modal-mode minor-mode-overriding-map-alist)))
  (run-hooks 'my-modal-state-change-hook)
  (force-mode-line-update t))




;; Turn on the mode
(my-modal-mode 1)

;;======================================================================= EOF ===========================================================
(provide 'my-modal)
;;; my-modal.el ends here
