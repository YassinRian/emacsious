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

;; Define the hydra
(pretty-hydra-define hydra-text-manipulation
  (:title "Text Manipulation" :quit-key "q" :color blue)
  ("Case"
   (("u" upcase-dwim "UPCASE")
    ("l" downcase-dwim "lowercase")
    ("c" capitalize-dwim "Capitalize")
    ("m" (lambda () (interactive) (my/transform-region-or-word #'my/camelize)) "camelCase")
    ("p" (lambda () (interactive) (my/transform-region-or-word #'my/pascal-case)) "PascalCase"))

   "Convert"
   (("s" (lambda () (interactive) (my/transform-region-or-word #'my/snake-case)) "snake_case")
    ("k" (lambda () (interactive) (my/transform-region-or-word #'my/kebab-case)) "kebab-case")
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
    ("R" reverse-region "Reverse"))))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c t") #'hydra-text-manipulation/body)

(provide 'config-text-manipulation)
;;; config-text-manipulation.el ends here
