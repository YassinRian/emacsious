;;; config-text-manipulation.el --- Text manipulation hydra -*- lexical-binding: t -*-

;;; Commentary:
;; Text manipulation functionality using Pretty-Hydra

;;; Code:

(require 'pretty-hydra)

;; Helper functions for text transformations
(defun my/camelize (s)
  "Convert string S to camelCase."
  (let* ((words (split-string s "[^a-zA-Z0-9]+" t))
         (first (downcase (car words)))
         (rest (mapcar #'capitalize (cdr words))))
    (apply #'concat first rest)))

(defun my/pascal-case (s)
  "Convert string S to PascalCase."
  (let ((words (split-string s "[^a-zA-Z0-9]+" t)))
    (apply #'concat (mapcar #'capitalize words))))

(defun my/snake-case (s)
  "Convert string S to snake_case."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2"
      (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" s)))))

(defun my/kebab-case (s)
  "Convert string S to kebab-case."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1-\\2"
      (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" s)))))

(defun my/indent-region-or-line (spaces)
  "Indent region if active, or current line, by SPACES spaces."
  (interactive "nNumber of spaces: ")
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) spaces)
    (let ((line-beginning (line-beginning-position))
          (line-end (line-end-position)))
      (indent-rigidly line-beginning line-end spaces))))

(defun my/indent-right (arg)
  "Indent region or line right by ARG spaces."
  (interactive "p")
  (my/indent-region-or-line arg))

(defun my/indent-left (arg)
  "Indent region or line left by ARG spaces."
  (interactive "p")
  (my/indent-region-or-line (- arg)))


;; Functions to apply transformations to region or word
(defun my/transform-region-or-word (transform-fn)
  "Apply TRANSFORM-FN to region if active, or word at point."
  (if (use-region-p)
      (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
             (transformed (funcall transform-fn text)))
        (delete-region (region-beginning) (region-end))
        (insert transformed))
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (text (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (transformed (funcall transform-fn text)))
      (delete-region (car bounds) (cdr bounds))
      (insert transformed))))

;; ================================ hydra's ====================================


;; Define the hydra
(pretty-hydra-define hydra-text-manipulation
  (:title "Text Manipulation" :quit-key "q" :color blue)
  ("Case"
   (("u" upcase-dwim "UPCASE")
    ("l" downcase-dwim "lowercase")
    ("c" capitalize-dwim "Capitalize")
    ("m" (lambda () (interactive) (my/transform-region-or-word #'my/camelize)) "camelCase")
    ("p" (lambda () (interactive) (my/transform-region-or-word #'my/pascal-case)) "PascalCase"))
   
"Indent"

((">" my-indent-rigidly-right "Indent right" :exit nil)
 ("<" my-indent-rigidly-left "Indent left" :exit nil))
   
   "Move"
   (("I" drag-stuff-up "Move up" :exit nil)
    ("O" drag-stuff-down "Move down" :exit nil))
   
   "Convert"
   (
    ("w" just-one-space "Single Space")
    ("W" delete-horizontal-space "No Space"))

   "Edit"
   (("d" duplicate-dwim "Duplicate")
    ("r" reverse-region "Reverse")
    ("t" transpose-words "Transpose")
    ("=" indent-region "Indent"))

   "Lines"
   (("j" join-line "Join")
    ("S" sort-lines "Sort")
    ("U" delete-duplicate-lines "Unique")
    ("R" reverse-region "Reverse"))

   ))


(provide 'config-text-manipulation)
;;; config-text-manipulation.el ends here
