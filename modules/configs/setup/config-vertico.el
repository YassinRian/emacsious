;;; config-vertico.el --- Completion system configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Unified configuration for completion system using:
;; - Vertico: Vertical completion UI
;; - Marginalia: Rich annotations
;; - Orderless: Flexible completion matching

;;; Code:

(require 'vertico)
(require 'marginalia)
(require 'orderless)
(require 'ef-themes)

;; ========================= Theme Integration ==============================

(defun my-completion-update-faces ()
  "Update completion faces based on current ef-theme."
  (custom-set-faces
   ;; Vertico faces
   `(vertico-current ((t (:background ,(ef-themes-get-color-value 'bg-inactive)
                         :foreground ,(ef-themes-get-color-value 'fg-main)
                         :extend t))))

   ;; Marginalia faces for file attributes
   `(marginalia-file-name ((t (:foreground ,(ef-themes-get-color-value 'fg-main)))))
   `(marginalia-file-modes ((t (:foreground ,(ef-themes-get-color-value 'fg-dim)))))
   `(marginalia-file-owner ((t (:foreground ,(ef-themes-get-color-value 'fg-alt)))))
   `(marginalia-size ((t (:foreground ,(ef-themes-get-color-value 'blue-cooler)))))
   `(marginalia-date ((t (:foreground ,(ef-themes-get-color-value 'green-cooler)))))
   `(marginalia-modified ((t (:foreground ,(ef-themes-get-color-value 'red)))))
   `(marginalia-installed ((t (:foreground ,(ef-themes-get-color-value 'magenta-cooler)))))))

;; Update faces when theme changes
(add-hook 'ef-themes-post-load-hook #'my-completion-update-faces)

;; ========================= Vertico Configuration ==============================

;; Basic settings
(setq vertico-cycle t           ; Enable cycling for `vertico-next' and `vertico-previous'
      vertico-resize nil        ; Keep minibuffer fixed size
      vertico-scroll-margin 0   ; Set scroll margin
      vertico-count 12          ; Show 12 candidates
      vertico-preselect 'prompt ; Start with prompt selected
      vertico-mouse-hide nil    ; Enable mouse support
      vertico-padding 1)        ; Add some padding

;; ========================= Marginalia Configuration ==========================

;; Configure annotators with cleaner display
(setq marginalia-annotator-registry
      (append marginalia-annotator-registry
              '((command marginalia-annotate-binding builtin none)
                (symbol marginalia-annotate-symbol builtin))))

;; Style settings
(setq marginalia-align-offset 8        ; Adjust space between columns
      marginalia-align 'right          ; Right-align annotations
      marginalia-max-relative-age 0    ; Always show absolute dates
      marginalia-field-width 80)       ; Wider field for annotations

;; Key binding for cycling annotations
(define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)

;; ========================= Orderless Configuration ==========================

;; Helper functions for pattern matching
(defun orderless-dispatch-flex-first (_pattern index _total)
  "Use flex matching on first pattern in completion."
  (and (eq index 0) 'orderless-flex))

(defun without-if-bang (pattern _index _total)
  "Match PATTERN as literal if it starts with '!'."
  (when (string-prefix-p "!" pattern)
    `(orderless-literal . ,(substring pattern 1))))

(defun flex-if-equal (pattern _index _total)
  "Match PATTERN as flex if it starts with '='."
  (when (string-prefix-p "=" pattern)
    `(orderless-flex . ,(substring pattern 1))))

(defun initialism-if-comma (pattern _index _total)
  "Match PATTERN as initialism if it starts with ','."
  (when (string-prefix-p "," pattern)
    `(orderless-initialism . ,(substring pattern 1))))

;; Configure matching styles and dispatchers
(setq orderless-matching-styles '(orderless-literal
                                 orderless-regexp
                                 orderless-initialism
                                 orderless-prefixes)
      orderless-style-dispatchers '(orderless-dispatch-flex-first
                                   without-if-bang
                                   flex-if-equal
                                   initialism-if-comma))

;; Set as default completion style
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;; ========================= Enable Components ============================

;; Initialize faces
(my-completion-update-faces)

;; Enable components
(vertico-mode 1)
(marginalia-mode 1)

(provide 'config-vertico)
;;; config-vertico.el ends here
