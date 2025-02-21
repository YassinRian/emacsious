;;; my-modeline.el --- Enhanced modeline with ef-themes integration

;;; Commentary:
;; Custom modeline implementation with modal state, git info, and buffer info
;; Integrates with ef-themes for consistent styling

;;; Code:

(require 'my-modal)
(require 'ef-themes)
(require 'vc)

;; =================================== Faces ====================================

(defgroup my-modeline nil
  "Custom modeline faces and configuration."
  :group 'mode-line)

;; Base modeline appearance
(defface my-modeline-base
  '((t (:inherit mode-line)))
  "Base face for modeline elements.")

(defface my-modeline-normal
  '((t (:inherit my-modeline-base
        :foreground "#50fa7b"
        :weight bold)))
  "Face for normal mode indicator.")

(defface my-modeline-insert
  '((t (:inherit my-modeline-base
        :foreground "#61afef"
        :weight bold)))
  "Face for insert mode indicator.")

(defface my-modeline-visual
  '((t (:inherit my-modeline-base
        :foreground "#ff79c6"
        :weight bold)))
  "Face for visual mode indicator.")

(defface my-modeline-modified
  '((t (:inherit my-modeline-base
        :foreground "#ff5555"
        :weight bold)))
  "Face for modified buffer indicator.")

(defface my-modeline-git
  '((t (:inherit my-modeline-base
        :foreground "#bd93f9"
        :weight bold)))
  "Face for git information.")

(defface my-modeline-dim
  '((t (:inherit my-modeline-base
        :foreground "#6272a4")))  ; A dimmer color for separators
  "Face for dimmed elements like separators.")

;; ============================ Helper Functions ================================


(defun my-modeline-state ()
  "Get the current modal state indicator."
  (let ((state (if (boundp 'my-modal-state)
                   my-modal-state
                 'insert)))  ; default to insert if state not set
    (pcase state
      ('normal (propertize "NORMAL" 'face 'my-modeline-normal))
      ('visual (propertize "VISUAL" 'face 'my-modeline-visual))
      ('insert (propertize "INSERT" 'face 'my-modeline-insert))
      ('yank   (propertize "YANK"   'face 'my-modeline-normal))
      ('delete (propertize "DELETE" 'face 'my-modeline-normal))
      ('menu1  (propertize "MENU1" 'face 'my-modeline-normal))
      (_       (propertize "INSERT" 'face 'my-modeline-insert)))))

;; Add hook to update modeline when state changes
(add-hook 'my-modal-state-change-hook #'force-mode-line-update)

(defun my-modeline-buffer-info ()
  "Get buffer info including modification state."
  (let ((mod-state (if (and (buffer-file-name)
                           (buffer-modified-p))
                       (propertize "●" 'face 'my-modeline-modified)
                     (propertize "○" 'face 'my-modeline-dim))))
    (concat mod-state " " (buffer-name))))

(defun my-modeline-git-info ()
  "Get git branch information if available."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (branch (when backend
                    (substring-no-properties vc-mode 
                                           (+ (if (eq backend 'Hg) 2 3) 2)))))
      (when branch
        (concat (propertize "  " 'face 'my-modeline-dim)
                (propertize branch 'face 'my-modeline-git))))))

(defun my-modeline-position ()
  "Get cursor position information."
  (let ((line (format "%d" (line-number-at-pos)))
        (col (format "%d" (current-column))))
    (concat (propertize "  " 'face 'my-modeline-dim)
            line
            (propertize ":" 'face 'my-modeline-dim)
            col)))

(defun my-modeline-sep ()
  "Return a separator with proper face."
  (propertize " | " 'face `(:foreground ,(ef-themes-get-color-value 'fg-dim))))

(defun my-modeline-pad (str)
  "Add consistent padding to STR."
  (concat " " str " "))

(defun my-modeline-update ()
  "Force modeline update."
  (force-mode-line-update))


;; ============================= Theme Integration ==============================

;; Ensure modeline inherits the correct background
(set-face-attribute 'mode-line nil
                   :background (face-background 'default)
                   :box nil)  ; Remove the box/border

(set-face-attribute 'mode-line-inactive nil
                   :background (face-background 'default)
                   :box nil)

;; Update the modeline faces function
(defun my-modeline-update-faces ()
  "Update modeline faces based on current ef-theme."
  (let ((theme-name (car custom-enabled-themes)))
    (when (string-prefix-p "ef-" (symbol-name theme-name))
      (let ((bg-main (face-background 'default)))
        ;; Update mode-line faces to match theme
        (set-face-attribute 'mode-line nil
                          :background bg-main
                          :box nil)
        (set-face-attribute 'mode-line-inactive nil
                          :background bg-main
                          :box nil)
        
        ;; Update your custom faces
        (set-face-attribute 'my-modeline-normal nil
                          :foreground (ef-themes-get-color-value 'green-warmer)
                          :background bg-main)
        (set-face-attribute 'my-modeline-insert nil
                          :foreground (ef-themes-get-color-value 'blue-warmer)
                          :background bg-main)
        (set-face-attribute 'my-modeline-visual nil
                          :foreground (ef-themes-get-color-value 'magenta)
                          :background bg-main)
        (set-face-attribute 'my-modeline-modified nil
                          :foreground (ef-themes-get-color-value 'red)
                          :background bg-main)
        (set-face-attribute 'my-modeline-git nil
                          :foreground (ef-themes-get-color-value 'purple-warmer)
                          :background bg-main)
        (set-face-attribute 'my-modeline-dim nil
                          :foreground (ef-themes-get-color-value 'fg-dim)
                          :background bg-main)))))

;; Make sure to call this after loading a theme
(add-hook 'ef-themes-post-load-hook #'my-modeline-update-faces)

;; You might also want to call it now if a theme is already loaded
(when (bound-and-true-p ef-themes-post-load-hook)
  (my-modeline-update-faces))

;; ============================= Modeline Format ===============================

;; In my-modeline.el

;; Update the modeline format with more explicit padding
(setq-default mode-line-format
              '((:eval
                 (concat
                  (propertize "  ")  ; Left margin
                  (my-modeline-pad (my-modeline-state))
                  (my-modeline-sep)
                  (my-modeline-buffer-info)
                  (my-modeline-git-info)
                  (my-modeline-position)
                  (propertize "  ")  ; Right margin
                  ))))
                  
(provide 'my-modeline)
;;; my-modeline.el ends here
