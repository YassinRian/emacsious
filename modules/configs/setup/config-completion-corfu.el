;; config-completion-corfu.el - Modern completion configuration

(require 'corfu)
(require 'kind-icon)
(require 'nerd-icons)
(require 'nerd-icons-corfu)
(require 'cape)
(require 'dabbrev)
(require 'corfu-popupinfo)
(require 'corfu-history)
(require 'corfu-echo)
(require 'corfu-quick)
(require 'corfu-terminal)
(require 'tempel)

;; Basic Corfu settings
(setq corfu-auto t
      corfu-quit-no-match t
      corfu-on-exact-match 'quit
      corfu-auto-delay 0
      corfu-auto-prefix 2
      corfu-popupinfo-delay '(0.5 . 0.2)
      corfu-preselect 'prompt
      corfu-margin-specs         ; Add more padding
   	'((margin . 0.2)           ; Increase left/right margin (default is 0.15)
     	  (padding-right . 0.4)    ; Add extra padding on the right
     	  (padding-left . 0.4))
      corfu-min-width 40        ; Set minimum width of completion window
      corfu-max-width 80        ; Set maximum width of completion window
      ;;corfu-count 14            ; Show more candidates
      corfu-scroll-margin 4     ; Show a few extra candidates below/above
      corfu-echo-documentation 0.25 ; Show docs quickly
      )
   

;; Custom sorting function
(defun mo-corfu-combined-sort (candidates)
  "Sort CANDIDATES using both display-sort-function and corfu-sort-function."
  (let ((candidates
         (let ((display-sort-func (corfu--metadata-get 'display-sort-function)))
           (if display-sort-func
               (funcall display-sort-func candidates)
             candidates))))
    (if corfu-sort-function
        (funcall corfu-sort-function candidates)
      candidates)))

(setq corfu-sort-override-function #'mo-corfu-combined-sort)

;; Corfu minibuffer and shell setup
(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if completion-at-point is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (setq-local corfu-echo-delay nil
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))

(defun mo-corfu-enable-no-auto()
  "Enable corfu without auto completion."
  (setq-local corfu-auto nil)
  (corfu-mode))

;; Shell integration
(defun corfu-send-shell (&rest _)
  "Send completion candidate when inside comint/eshell."
  (cond
   ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
    (eshell-send-input))
   ((and (derived-mode-p 'comint-mode) (fboundp 'comint-send-input))
    (comint-send-input))))

;; Key bindings
(define-key corfu-map (kbd "SPC") #'corfu-insert-separator)
(define-key corfu-map (kbd "TAB") #'corfu-next)
(define-key corfu-map [tab] #'corfu-next)
(define-key corfu-map (kbd "RET") nil)
(define-key corfu-map [remap next-line] nil)
(define-key corfu-map [remap previous-line] nil)
(define-key corfu-map (kbd "C-l") #'corfu-next)
(define-key corfu-map (kbd "C-k") #'corfu-previous)
(define-key corfu-map (kbd "C-;") #'corfu-quick-complete)

;; Ensure proper icon display in terminal
(setq corfu-terminal-mode t)  ; Enable terminal support

;; Kind-icon configuration
;;(setq kind-icon-default-face 'corfu-default
;;      kind-icon-default-height 0.38
 ;;     kind-icon-blend-background t
 ;;     kind-icon-blend-frac 0.12
  ;;    kind-icon-use-icons t)
      
;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)



;; Enable various Corfu extensions
(global-corfu-mode)
(corfu-history-mode 1)
(corfu-popupinfo-mode 1)
(corfu-echo-mode 1)

;; Terminal support  
(unless (display-graphic-p)
    (corfu-terminal-mode))
(when (not (display-graphic-p))
  (setq nerd-icons-font-family nil))


;; History persistence
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'corfu-history)

;; Cape configuration
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-abbrev)
(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-line)
(add-to-list 'completion-at-point-functions #'cape-emoji)

;; Silence the pcomplete capf, no errors or messages!
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

;; Configure Dabbrev
(setq dabbrev-case-fold-search t)        ; Case insensitive search
(setq dabbrev-case-replace nil)          ; Preserve case when replacing
(setq dabbrev-upcase-means-case-search t) ; Upcase triggers case-sensitive search

;; More intelligent Dabbrev completion
(setq cape-dabbrev-min-length 3)         ; Min chars for dabbrev expansion
(setq cape-dabbrev-check-other-buffers t) ; Search in other buffers

;; Dabbrev configuration
(setq dabbrev-ignored-buffer-regexps '("^\\*.+::stderr\\*$")
      dabbrev-check-all-buffers nil)
(defvar mo-dabbrev-max-file-size 1000000)
(setq dabbrev-friend-buffer-function
      (lambda (buffer)
        (< (buffer-size buffer) mo-dabbrev-max-file-size)))

;; Set up hooks
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
(add-hook 'eshell-mode-hook #'mo-corfu-enable-no-auto)
(add-hook 'shell-mode-hook #'mo-corfu-enable-no-auto)

;; Add advice for shell integration and separator handling
(advice-add #'corfu-insert :after #'corfu-send-shell)
(advice-add 'corfu-insert-separator :before
            #'(lambda ()
                (if (>= corfu--index 0)
                    (progn
                      (corfu-complete)
                      (insert "")))))

;; In your config-corfu.el
(advice-add 'corfu-insert-separator :before
            #'(lambda ()
                (if (>= corfu--index 0)
                    (progn
                      (corfu-complete)
                      (insert "")))))

    
(provide 'config-completion-corfu)
