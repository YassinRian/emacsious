;;; -*- lexical-binding: t; -*-

(require 'my-modal)
(require 'boon-moves)
(require 'expreg)
(require 'custom-functions)
(require 'phi-search)
(require 'pretty-hydras)
(require 'vundo)

;; ================================================================ Keybindings =======================================

;; ================================================================ Insert Mode Keybindings** 


;; Make ; move forward one character in your modal map
;;(define-key my-modal-insert-map (kbd ";") #'forward-char)

(define-key my-modal-insert-map (kbd ";") 'smart-semicolon)

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
(define-key my-modal-normal-map (kbd "<") #'beginning-of-buffer)
(define-key my-modal-normal-map (kbd ">") #'end-of-buffer)
(define-key my-modal-normal-map (kbd "/") #'consult-line)
(define-key my-modal-normal-map (kbd "v") ;; insert mode 
  (lambda () 
    (interactive)
    (my/push-mark-with-feedback)
    (my-modal-enter-insert-mode)
    ;; Check if we're in eshell mode and activate the appropriate map
    (when (eq major-mode 'eshell-mode)
      (my-modal-eshell-activate-map))))
;;(define-key my-modal-normal-map (kbd "v") #'my-modal-enter-insert-mode) ;; Insert mode
(define-key my-modal-normal-map (kbd "SPC") #'my-modal-enter-visual-mode) ;; Visual mode
(define-key my-modal-normal-map (kbd "c") #'my-modal-enter-c-mode)
(define-key my-modal-normal-map (kbd "d") #'my-modal-enter-delete-mode)
(define-key my-modal-normal-map (kbd "w") #'start-menu/body) ;; start menu
(define-key my-modal-normal-map (kbd "g") #'hydra-goto/body) ;; start menu

;; avy
(define-key my-modal-normal-map (kbd "f") #'avy-goto-char-in-line) ;; start menu

;; Redo bindings
(define-key my-modal-normal-map (kbd "r") #'undo-fu-only-undo)
(define-key my-modal-normal-map (kbd "R") #'undo-fu-only-redo)

;; Scroll
(define-key my-modal-normal-map (kbd "S-M-k") #'scroll-up-line)
(define-key my-modal-normal-map (kbd "S-M-l") #'scroll-down-line)

;; Smooth line scrolling
(define-key my-modal-normal-map (kbd "M-i") #'scroll-up-command)
(define-key my-modal-normal-map (kbd "M-o") #'scroll-down-command)

;; Marks
(define-key my-modal-normal-map (kbd "n") #'my/consult-mark-backward)
(define-key my-modal-normal-map (kbd "N") #'my/consult-mark-forward)
(define-key my-modal-normal-map (kbd ":") #'my/push-mark-with-feedback)

;; =============================================================== Visual Mode Keybindings**
(define-key my-modal-visual-map (kbd "j") #'boon-smarter-backward)
(define-key my-modal-visual-map (kbd ";") #'boon-smarter-forward)
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
(define-key my-modal-visual-map (kbd "s") #'smart-select-string)
(define-key my-modal-visual-map (kbd "SPC") #'my-visual-select-line)
(define-key my-modal-visual-map (kbd "n") #'exchange-point-and-mark)

;; Bind this wrapper function to ai
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

;; "Text manipulation"
(define-key my-modal-visual-map (kbd "t") #'hydra-text-manipulation/body)

;; ============================================================= C mode **

(define-key my-modal-c-map (kbd "p") #'visual-paste)
(define-key my-modal-c-map (kbd "e") 'emmet-expand-line)

;; ============================================================== Delete mode **

(define-key my-modal-delete-map (kbd "j") #'my-delete-word-backward-safe)
(define-key my-modal-delete-map (kbd ";") #'my-delete-word-forward-safe)
(define-key my-modal-delete-map (kbd "k") #'my-delete-char-left)
(define-key my-modal-delete-map (kbd "l") #'my-delete-char-right)
(define-key my-modal-delete-map (kbd "c") #'my-delete-char-at-point)
(define-key my-modal-delete-map (kbd "w") #'delete-word-at-cursor)
(define-key my-modal-delete-map (kbd "s") #'my-delete-extended-string)
(define-key my-modal-delete-map (kbd "i") #'delete-between-equal-chars)
(define-key my-modal-delete-map (kbd "a") #'delete-including-equal-chars)


;; Line operations
(define-key my-modal-delete-map (kbd "p") #'my-delete-line-end)      ; delete to line end
(define-key my-modal-delete-map (kbd "u") #'my-delete-line-start)    ; delete to line start
;;(define-key my-modal-delete-map (kbd "SPC") #'kill-whole-line)     ; delete entire line
;; Option 2: Use a lambda in the keybinding
(define-key my-modal-delete-map (kbd "SPC") ;; handy to have this end on normal-mode
  (lambda ()
    (interactive)
    (kill-whole-line)
    (my-modal-enter-normal-mode)))

;; =============================================================== Switching to Normal Mode **

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

(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; Disable the minor mode when leaving dired-mode
(add-hook 'dired-mode-hook 
          (lambda ()
            (add-hook 'change-major-mode-hook
                     (lambda () (my-dired-navigation-mode -1))
                     nil t)))
                     

;; =============================================================== Other keybindings **
 
;; ==== viper **

(define-key my-modal-normal-map (kbd ":") #'viper-ex)

;; ===== vertico easy navigation **

(define-key vertico-map (kbd "f") #'hydra-vertico-mode/body)  
;;(define-key vertico-map (kbd "C-p") #'vertico-directory-up)  
;;(define-key global-map (kbd "C-p") #'vertico-directory-up)


(defun my-activate-vertico-nav ()
  "Activate transient navigation map for vertico."
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'next-line)
    (define-key map (kbd "i") #'previous-line)
    (set-transient-map map t)))


;; Init vertico-directory for directory navigation commands
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map ("M-<backspace>" . vertico-directory-up))) ;; C-<backspace> doesnt work because of alacritty

;; Init vertico-multiform for per command vertico configuration
(use-package vertico-multiform
  :after vertico
  :ensure nil
  :config
  (vertico-multiform-mode))


;; ===== corfu easy navigation **

(define-key corfu-map (kbd ";") #'my-corfu-next)
(define-key corfu-map (kbd "SPC") #'corfu-insert-separator)

(defun my-corfu-next ()
  "corfu-next"
  (interactive)
  (corfu-next))

(advice-add 'my-corfu-next :after 
            (lambda (&rest args)
              (call-interactively 'my-activate-corfu-nav)))

(advice-add 'corfu-quick-complete :around
            (lambda (orig-fun &rest args)
              (let ((result (apply orig-fun args)))
                ;; Quit Corfu
                (corfu-quit)
                ;; Ensure the transient map from my-activate-corfu-nav is removed
                ;; This effectively deactivates the function by removing its keymap
                (set-transient-map nil)
                result)))

(defun my-activate-corfu-nav ()
  "Activate transient navigation map for corfu with fallback for other keys."
  (interactive)
  (let ((map (make-sparse-keymap)))
    ;; Define your specific keybindings
    (define-key map (kbd "i") #'corfu-previous)
    (define-key map (kbd "o") #'corfu-next)
    
    ;; Set a default binding for any other key
    (set-keymap-parent map (make-composed-keymap nil corfu-mode-map))
    
    ;; Create a function that handles all other keys by first completing then executing the original command
    (define-key map [t] (lambda ()
                          (interactive)
                          (corfu-complete)
                          (let ((keys (this-command-keys-vector)))
                            (setq unread-command-events (listify-key-sequence keys)))))
    
    (set-transient-map map t #'identity)))


;; ===== Phi-search **

(define-key phi-search-default-map (kbd "f") #'hydra-phi-mode/body) 

(defun my-activate-phi-nav ()
  "Activate transient navigation map for phi."
  (interactive)
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "i") #'phi-search-again-or-previous)
	(define-key map (kbd "o") #'phi-search-again-or-next)
	(define-key map (kbd "s") #'phi-search-quick-occur)
	   ;; Add a key to switch back to phi-search occur results if they exist
    (define-key map (kbd "r") 
                (lambda ()
                  (interactive)
                  (let ((occur-buf (get-buffer "*Phi-Occur*")))
                    (when occur-buf
                      (select-window (display-buffer occur-buf))))))
    (set-transient-map map t #'identity)))
    	      
;; ======== vundo **

(define-key vundo-mode-map (kbd ";") #'vundo-forward)
(define-key vundo-mode-map (kbd "j") #'vundo-backward)
(define-key vundo-mode-map (kbd "o") #'vundo-next)
(define-key vundo-mode-map (kbd "i") #'vundo-previous)
(define-key vundo-mode-map (kbd "u") #'vundo-stem-root)
(define-key vundo-mode-map (kbd "p") #'vundo-stem-end)
(define-key vundo-mode-map (kbd "q") #'vundo-quit)
(define-key vundo-mode-map (kbd "RET") #'vundo-confirm)


;; ========================== Global **

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; =========================== Eshell **
(require 'eshell)
(require 'em-prompt)

;; Simple function to activate eshell-mode-map in insert mode
(defun my-modal-eshell-activate-map ()
  "Activate eshell-mode-map when in insert mode."
  (when (and (eq major-mode 'eshell-mode)
             (eq my-modal-state 'insert))
    (use-local-map eshell-mode-map)
    ;; Add keybinding to enter normal mode
    (local-set-key (kbd "f") 'hydra-eshell-mode/body)))
    

;; Add hook to modal state change
(add-hook 'my-modal-state-change-hook 'my-modal-eshell-activate-map)

;; More robust eshell mode startup handling
(defun my-modal-eshell-mode-setup ()
  "Set up insert mode and correct keymap for eshell."
  ;; Ensure we're in eshell mode
  (when (eq major-mode 'eshell-mode)
    ;; Give a small delay to ensure eshell is fully initialized
    (run-with-timer 0.1 nil
                   (lambda ()
                     ;; Force insert mode
                     (my-modal-enter-insert-mode)
                     ;; Activate eshell map
                     (use-local-map eshell-mode-map)
                     ;; Add escape binding
                     (local-set-key (kbd "f") 'hydra-eshell-mode/body)))))

;; Add to both hooks for better coverage
(add-hook 'eshell-mode-hook 'my-modal-eshell-mode-setup)
(with-eval-after-load 'eshell
  (add-hook 'eshell-first-time-mode-hook 'my-modal-eshell-mode-setup))


;;================================================================================================== EOF =====================================================================================
(provide 'my-keybindings)
