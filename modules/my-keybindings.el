(require 'my-modal)
(require 'my-functions)
(require 'boon-moves)
(require 'expreg)
(require 'custom-functions)


;; ================================================================ Normal Mode Keybindings**

(define-key my-modal-normal-map (kbd "<remap> <self-insert-command>") #'ignore) ;; ignore other keys
(define-key my-modal-normal-map (kbd "j") #'boon-smarter-backward)
(define-key my-modal-normal-map (kbd ";") #'boon-smarter-forward)
(define-key my-modal-normal-map (kbd "o") #'next-line)
(define-key my-modal-normal-map (kbd "i") #'previous-line)
(define-key my-modal-normal-map (kbd "k") #'backward-char)
(define-key my-modal-normal-map (kbd "l") #'forward-char)
(define-key my-modal-normal-map (kbd "K") #'boon-smarter-upward)
(define-key my-modal-normal-map (kbd "L") #'boon-smarter-downward)
(define-key my-modal-normal-map (kbd "I") #'backward-paragraph)
(define-key my-modal-normal-map (kbd "O") #'forward-paragraph)
(define-key my-modal-normal-map (kbd "u") #'beginning-of-line)
(define-key my-modal-normal-map (kbd "p") #'end-of-line)

(define-key my-modal-normal-map (kbd "v") #'my-modal-enter-insert-mode) ;; Insert mode
(define-key my-modal-normal-map (kbd "SPC") #'my-modal-enter-visual-mode) ;; Visual mode
(define-key my-modal-normal-map (kbd "c") #'my-modal-enter-yank-mode)
(define-key my-modal-normal-map (kbd "d") #'my-modal-enter-delete-mode)
(define-key my-modal-normal-map (kbd "w") #'my-modal-enter-menu1-mode)

;; Redo bindings
(define-key my-modal-normal-map (kbd "r") #'my-enhanced-undo)
(define-key my-modal-normal-map (kbd "R") #'my-enhanced-redo)

;; Scroll
(define-key my-modal-normal-map (kbd "S-M-k") #'scroll-up-line)
(define-key my-modal-normal-map (kbd "S-M-l") #'scroll-down-line)

;; Smooth line scrolling
(define-key my-modal-normal-map (kbd "M-i") #'scroll-up-command)
(define-key my-modal-normal-map (kbd "M-o") #'scroll-down-command)

 
;; =============================================================== Visual Mode Keybindings**
(define-key my-modal-visual-map (kbd "j") #'backward-word)
(define-key my-modal-visual-map (kbd ";") #'forward-word)
(define-key my-modal-visual-map (kbd "k") #'backward-char)
(define-key my-modal-visual-map (kbd "l") #'forward-char)
(define-key my-modal-visual-map (kbd "i") #'previous-line)
(define-key my-modal-visual-map (kbd "o") #'next-line)
(define-key my-modal-visual-map (kbd "u") #'beginning-of-line)
(define-key my-modal-visual-map (kbd "p") #'end-of-line)

;; Delete in visual mode
(define-key my-modal-delete-map [t] #'my-modal-delete-mode-handle-unbound)
(define-key my-modal-visual-map (kbd "d") #'my-visual-delete) ;; roept een functie aan die alles verwijderd
(define-key my-modal-visual-map (kbd "c") #'visual-yank)

;;expand
(define-key my-modal-visual-map (kbd "e") #'expreg-expand)
(define-key my-modal-visual-map (kbd "r") #'expreg-contract)

;; select
(define-key my-modal-visual-map (kbd "w") #'my-visual-select-word)
(define-key my-modal-visual-map (kbd "h") #'my-visual-select-paragraph)
(define-key my-modal-visual-map (kbd "s") #'my-visual-select-between-spaces)
(define-key my-modal-visual-map (kbd "SPC") #'my-visual-select-line)
(define-key my-modal-visual-map (kbd "ai") #'visual-between-equal-chars)
(define-key my-modal-visual-map (kbd "aa") #'visual-including-equal-chars)

;; "Add surrounding keybindings to visual mode."
(define-key my-modal-visual-map (kbd "1") #'my-visual-surround-1)
(define-key my-modal-visual-map (kbd "2") #'my-visual-surround-2)
(define-key my-modal-visual-map (kbd "3") #'my-visual-surround-3)
(define-key my-modal-visual-map (kbd "4") #'my-visual-surround-4)
(define-key my-modal-visual-map (kbd "5") #'my-visual-surround-5)
(define-key my-modal-visual-map (kbd "6") #'my-visual-surround-6)
(define-key my-modal-visual-map (kbd "7") #'my-visual-surround-7)
(define-key my-modal-visual-map (kbd "8") #'my-visual-surround-8)
(define-key my-modal-visual-map (kbd "9") #'my-visual-surround-9)
(define-key my-modal-visual-map (kbd "0") #'my-visual-surround-0)


;; ============================================================= Yank mode **
(define-key my-modal-yank-map (kbd "p") #'visual-paste)


;; ============================================================== Delete mode **

(define-key my-modal-delete-map (kbd "j") #'my-delete-word-backward-safe)
(define-key my-modal-delete-map (kbd ";") #'my-delete-word-forward-safe)
(define-key my-modal-delete-map (kbd "k") #'my-delete-char-left)
(define-key my-modal-delete-map (kbd "l") #'my-delete-char-right)
(define-key my-modal-delete-map (kbd "c") #'my-delete-char-at-point)

;; Line operations
(define-key my-modal-delete-map (kbd "p") #'my-delete-line-end)      ; delete to line end
(define-key my-modal-delete-map (kbd "u") #'my-delete-line-start)    ; delete to line start
(define-key my-modal-delete-map (kbd "SPC") #'kill-whole-line)         ; delete entire line


;; ============================================================== Menu1 mode **

;; Add at least one binding or an escape hatch
(define-key my-modal-menu1-map (kbd "<remap> <self-insert-command>") #'ignore) ;; ignore other keys
(define-key my-modal-menu1-map (kbd "<escape>") #'my-modal-enter-normal-mode)
(define-key my-modal-menu1-map (kbd "s") #'hydra-search/body)


;; =============================================================== Global keybinding **
(global-set-key (kbd "f") 'hydra-change-mode/body)

;; =============================================================== Dired Mode **
(define-minor-mode my-dired-navigation-mode
  "Minor mode for custom dired navigation."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "o") #'next-line)
            (define-key map (kbd "i") #'previous-line)
            (define-key map (kbd "j") #'dired-up-directory)
            (define-key map (kbd ";") #'my-dired-open)
            (define-key map (kbd "SPC") #'hydra-dired/body)
            map))

;; Enable the minor mode when entering dired-mode
(add-hook 'dired-mode-hook #'my-dired-navigation-mode)

;; Disable the minor mode when leaving dired-mode
(add-hook 'dired-mode-hook 
          (lambda ()
            (add-hook 'change-major-mode-hook
                     (lambda () (my-dired-navigation-mode -1))
                     nil t)))

;; =============================================================== Other keybindings **

;;viper
(define-key my-modal-normal-map (kbd ":") #'viper-ex)

;; vertico easy navigation
(define-key vertico-map (kbd "f") #'hydra-vertico-mode/body)  

(defun my-activate-vertico-nav ()
  "Activate transient navigation map for vertico."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'next-line)
    (define-key map (kbd "i") #'previous-line)
    (set-transient-map map t)))

(defhydra hydra-vertico-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (my-activate-vertico-nav))))



(provide 'my-keybindings)
