;;; -*- lexical-binding: t -*-

;; Couple of handy function

(require 'hydra)
(require 'pretty-hydra)
(require 'phi-search)
(require 'config-snippets)

;; ======================== help functions

(defvar my-temporary-hydra-timer nil
  "Timer object for the timeout of my-temporary-hydra.")

(defvar my-temporary-hydra-option-selected nil
  "Flag to indicate if an option has been selected in my-temporary-hydra.")

(defvar my-temporary-original-cursor-color nil
  "Variable to store the original cursor color.")

(defvar my-hydra-message nil
  "variable to store the hydra message.")

(defun my-temporary-hydra-timeout (activation-char hydra-symbol)
  "Function to execute when a temporary hydra times out.
Inserts ACTIVATION-CHAR unless an option has been selected.
HYDRA-SYMBOL is the symbol representing the hydra."
  (setq my-temporary-hydra-timer nil)
  ;; Deactivate Hydra
  (when (bound-and-true-p hydra-symbol)
    (when (hydra-get-property hydra-symbol :is-active)
      (hydra-deactivate hydra-symbol)))
  ;; Insert activation character if no option has been selected
  (unless my-temporary-hydra-option-selected
    (insert (char-to-string activation-char))
    (setq hydra-deactivate t)
    (hydra-keyboard-quit)))

;; Handy if we want to add a extra hydra menu through a hydra for SPC
;;(bind-key "SPC" '(lambda () (interactive) (my-temporary-hydra-wrapper ?\s 'hydra-files/body 0.5)) boon-command-map)

;; (defun my-temporary-hydra-timeout (activation-char hydra-symbol)
;;   "Function to execute when a temporary hydra times out.
;; Inserts ACTIVATION-CHAR unless an option has been selected.
;; HYDRA-SYMBOL is the symbol representing the hydra."
;;   (setq my-temporary-hydra-timer nil)
;;   ;; Deactivate Hydra
;;   (when (bound-and-true-p hydra-symbol)
;;     (when (hydra-get-property hydra-symbol :is-active)
;;       (hydra-deactivate hydra-symbol)))
;;   ;; Insert activation character if no option has been selected
;;   (unless my-temporary-hydra-option-selected
;;     (if (eq activation-char ?\s)
;; 	(boon-drop-mark)
;;       (insert (char-to-string activation-char)))
;;     (setq hydra-deactivate t)
;;     (hydra-keyboard-quit)))


(defun my-temporary-hydra-wrapper (activation-char hydra timeout)
  "Create a temporary hydra with options and remove it after a timeout if no option is selected within TIMEOUT seconds.
ACTIVATION-CHAR is the character to be inserted upon timeout.
HYDRA is the pre-defined hydra symbol.
TIMEOUT is the time to wait before timing out."
  (interactive "cActivation character: \nXHydra body: \nnTimeout (seconds): ")
  (setq my-temporary-hydra-option-selected nil)
  (let* ((timeout-seconds (truncate timeout))  ;; Integer part of the timeout
         (timeout-milliseconds (truncate (* 1000 (mod timeout 1))))  ;; Milliseconds part of the timeout
         (timeout-fraction (truncate (* 1000 (mod timeout 1))))  ;; Fractional part of the timeout
         (timeout-string (format "%d.%d" timeout-seconds timeout-fraction)))  ;; Combined seconds and milliseconds
    (setf my-temporary-original-cursor-color (frame-parameter nil 'cursor-color))
    (setq my-temporary-hydra-timer
          (run-at-time
           timeout-string
           nil
           #'my-temporary-hydra-timeout
           activation-char hydra)))
  (funcall hydra) ; Assuming `hydra` is the symbol of the hydra definition
  )




;; ===================== Modal hydra **

(defhydra hydra-change-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (corfu-quit)
	     (my-modal-enter-normal-mode))))
   
;; ====================== start menu **

(pretty-hydra-define start-menu (:title "✦ ✦ ✦ START MENU ✦ ✦ ✦" :quit-key "q" :color blue)
  (
   "Files"   
    (
     ("t" (setq my-find-current-window (not my-find-current-window)) "Toggle Find file" :exit nil)
     ("f" (if my-find-current-window
           (my/consult-find-sort-by-depth-rg-fast-select)
           (my/find-file-split-right))
      (format "Find file %s" (if my-find-current-window "[↓ Here]" "[→ Split]")) :exit t)
     ("r" consult-recent-file "Recent files")
     
     )
    "Project"
    (("p" my/set-consult-find-dir-with-consult "Choose project dir" :exit t)
     ("SPC" my/consult-find-sort-by-depth-rg-fast "Quick files Search" :exit t)
     ("R" my/reset-consult-find-dir "Reset to default" :exit nil) ;; after reset the function makes use of the Toggle find file ("t")
     )
    
   "Window"
   (("v" my-split-window-right "Split vertical" :exit t)
    ("-" my-split-window-below "Split Horiz")
    ("d" delete-window "Delete")
    ("m" delete-other-windows "Maximize")
    )
   
   "Search"
   (
    ("/" phi-search "Search")
    ("s" hydra-search/body "Advanced Search") ;; see config-search
    )
   
   "Yanks & Marks"
   (("y" consult-yank-pop "Get copied text"))

   "Advanced funcs"
   (
    ("w" hydra-windows/body "Files & Windows") ;; see config-buffers
    ("M" mark-management-hydra/body "Mark funcs")
    ("T" hydra-text-manipulation/body "Text")
    ("." hydra-snippets/body "Snippets")
    )
   
  ))
  
;; ==================== Goto Menu **

(defhydra hydra-goto (:hint nil)
  ("w" ace-window :exit t)
  ("SPC" consult-buffer :exit t)
  ("j" join-line :exit nil)
  ("d" dired :exit t))


;; ===================== phi mode **

(defhydra hydra-phi-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (my-activate-phi-nav))))
	      
;; ===================== vertico mode **
	      
(defhydra hydra-vertico-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (my-activate-vertico-nav))))
	      
;; ===================== corfu mode **
	      
(defhydra hydra-corfu-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (my-activate-corfu-nav))))


;; ===================== Insert mode **

(defhydra hydra-insert-mode (:color blue :idle 1.0 :timeout 0.5)
  ("f" (forward-char))
  ("w" (boon-open-line-and-insert) :exit t)
  ("s" (boon-open-next-line-and-insert) :exit t)
  (";" (insert ";") :exit t)
  )

;; =========================== Eshell **
	      
(defhydra hydra-eshell-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (my-modal-enter-normal-mode))))

;; =========================== flycheck **

;; Define a pretty-hydra for Flake8 functionality
(pretty-hydra-define flake8-hydra
  (:title "Flake8 Commands" :color blue :quit-key "q")
  ("Navigation"
   (("n" flycheck-next-error "Next error" :exit nil)
    ("p" flycheck-previous-error "Previous error" :exit nil)
    ("f" flycheck-first-error "First error" :exit nil)
    ("l" flycheck-list-errors "List errors" :exit t))

   "Check"
   (("c" flycheck-buffer "Check buffer" :exit nil)
    ("C" flycheck-clear "Clear errors" :exit nil)
    ("v" flycheck-verify-setup "Verify setup" :exit t)
    ("s" flycheck-select-checker "Select checker" :exit t))
   
   "Config"
   (("m" (lambda () (interactive) (find-file (concat (projectile-project-root) ".flake8"))) "Edit .flake8 config" :exit t)
    ("d" (lambda () (interactive) (customize-group 'flycheck)) "Customize flycheck" :exit t)
    ("r" flycheck-compile "Run manual check" :exit nil))
   
   "Django specific"
   (("e" (lambda () (interactive) (flycheck-disable-checker 'python-flake8)) "Disable flake8" :exit nil)
    ("E" (lambda () (interactive) (flycheck-enable-checker 'python-flake8)) "Enable flake8" :exit nil)
    ("i" (lambda () (interactive) 
           (let ((flycheck-flake8-ignored-errors 
                  (append flycheck-flake8-ignored-errors '("DJ01"))))
             (flycheck-buffer))) "Ignore Django warnings" :exit nil))))


;; ========================================================================== EOF ============================================================
(provide 'pretty-hydras)
