;;; -*- lexical-binding: t -*-

;; Couple of handy function

(require 'hydra)
(require 'pretty-hydra)
(require 'phi-search)

;; ===================== Modal hydra **

(defhydra hydra-change-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
  	      (my-corfu-quit)
	      (my-modal-enter-normal-mode))))
   

;; ====================== start menu **

(pretty-hydra-define start-menu (:title "✦ ✦ ✦ START MENU ✦ ✦ ✦" :quit-key "q" :color blue)
  (
   "Files & Buffers"   
    (
     ("t" (setq my-find-current-window (not my-find-current-window)) "Toggle Find file" :exit nil)
     ("SPC" consult-buffer "Switch buffers" :exit t)
     ("f" (if my-find-current-window
           (my/consult-find-sort-by-depth-rg-fast)
           (my/find-file-split-right))
      (format "Find file %s" (if my-find-current-window "[↓ Here]" "[→ Split]")) :exit t)
     ;; Hidden toggle command
     ("r" consult-recent-file "Recent files")
     ("w" my-main-hydra/body "Files & Windows") ;; see config-buffers
   )

   "Window"
   (("v" split-window-right "Split vertical" :exit t)
    ("j" windmove-left "Move left" :exit t)
    (";" windmove-right "Move right" :exit t))

   "Search"
   (
    ("/" phi-search "Search")
    ("s" hydra-search/body "Advanced Search") ;; see config-search
    )
   
   "Yanks & Marks"
   (("y" consult-yank-pop "Get copied text"))
   
   "Editing"
   (("T" hydra-text-manipulation/body "Text"))
  ))
  
;; ==================== Goto Menu **

(defhydra hydra-goto (:hint nil :color blue)
  ("w" other-window :exit nil)) 


;; ===================== phi mode **

(defhydra hydra-phi-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (my-activate-phi-nav))))
	      
;; ===================== dired mode **
	      
(defhydra hydra-vertico-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	      (my-activate-vertico-nav))))
	      


;; ========================================================================== EOF ============================================================
(provide 'pretty-hydras)
