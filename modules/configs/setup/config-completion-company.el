;; config-completion.el - Modern completion configuration

(require 'company)
(require 'company-box)
(require 'all-the-icons)
(require 'company-quickhelp)
(require 'yasnippet)
(require 'kind-icon)
(require 'company-fuzzy)
(require 'company-prescient)



;; Basic company settings
(setq company-minimum-prefix-length 1
      company-idle-delay 0.1
      company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-tooltip-limit 10
      company-tooltip-maximum-width 80
      company-tooltip-minimum-width 30
      company-require-match nil
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-selection-default nil)

;; Company fuzzy settings
(setq company-fuzzy-sorting-backend 'flx
      company-fuzzy-prefix-on-top nil
      company-fuzzy-trigger-symbols '("`" "." ">" "<" "\"" "'" "/"))

;; Custom space handling
(defun my/company-space ()
  "Handle space in company-mode.
Complete if item selected, otherwise use as fuzzy separator."
  (interactive)
  (if (and company-selection-changed
           (> company-selection -1))
      (progn
        (company-complete)
        (insert " "))
    (progn
      (insert " ")
      (company-manual-begin))))

;; Key bindings
(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map [tab] 'company-select-next)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map [backtab] 'company-select-previous)
  (define-key company-active-map (kbd "SPC") 'my/company-space)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map [remap previous-line] nil)
  (define-key company-active-map [remap next-line] nil)
  (define-key company-active-map (kbd "C-l") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous))






;; Custom faces for company tooltips
(custom-set-faces
 '(company-tooltip ((t (:inherit default :background "#1a1b26" :foreground "#a9b1d6"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :foreground "#7dcfff" :bold t))))
 '(company-tooltip-selection ((t (:background "#24283b" :foreground "#c0caf5"))))
 '(company-tooltip-annotation ((t (:foreground "#9ece6a"))))
 '(company-tooltip-annotation-selection ((t (:foreground "#9ece6a"))))
 '(company-scrollbar-fg ((t (:background "#787c99"))))
 '(company-scrollbar-bg ((t (:background "#24283b")))))

;; Enable and configure company-box
(add-hook 'company-mode-hook 'company-box-mode)

;; Company-box settings
(setq company-box-icons-alist 'company-box-icons-all-the-icons)
(setq company-box-doc-enable t)
(setq company-box-doc-delay 0.3)
(setq company-box-scrollbar t)

;; Configure company-box icons
(setq company-box-icons-all-the-icons
      `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
        (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
        (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05))
        (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05))
        (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05))
        (Field . ,(all-the-icons-octicon "gear" :height 0.85 :v-adjust -0.05))
        (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust -0.05))
        (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2))
        (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2))
        (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2))
        (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
        (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
        (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2))
        (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2))
        (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
        (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
        (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
        (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
        (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
        (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
        (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2))
        (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
        (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2))
        (Event . ,(all-the-icons-octicon "zap" :height 0.85 :v-adjust -0.05))
        (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
        (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
        (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))

;; Enable company globally
(global-company-mode 1)












;; Configure kind-icon
(setq kind-icon-default-face 'company-tooltip)
(setq kind-icon-blend-background nil)
(setq kind-icon-blend-frac 0.08)

;; Company-box frame parameters
(setq company-box-frame-parameters
      '((left-fringe . 8)
        (right-fringe . 8)
        (line-spacing . 2)))

;; Enable and configure company-quickhelp
(company-quickhelp-mode 1)
(setq company-quickhelp-delay 0.5)
(setq company-quickhelp-max-lines 20)
(setq company-quickhelp-use-propertized-text t)

;; Enable company-prescient for better sorting
(company-prescient-mode 1)
(setq company-prescient-sort-length-enable nil)

;; Configure yasnippet
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)

;; Add yasnippet to company backends globally
(setq company-backends
      '((company-capf
         company-files
         company-yasnippet)
        (company-dabbrev-code
         company-keywords)
        company-dabbrev))

(provide 'config-completion-company)
