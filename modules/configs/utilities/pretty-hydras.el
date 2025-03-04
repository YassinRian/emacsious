;;; -*- lexical-binding: t -*-

;; Couple of handy function

(require 'hydra)
(require 'pretty-hydra)
(require 'phi-search)

;; ===================== Modal hydra **

(defhydra hydra-change-mode (:color blue :body-pre (insert "f") :idle 1.0 :timeout 0.5)
  ("d" (progn (delete-char -1)
	     (my-modal-enter-normal-mode))))
   
;; ====================== start menu **

(pretty-hydra-define start-menu (:title "✦ ✦ ✦ START MENU ✦ ✦ ✦" :quit-key "q" :color blue)
  (
   "Files & Buffers"   
    (
     ("t" (setq my-find-current-window (not my-find-current-window)) "Toggle Find file" :exit nil)
     ("SPC" my/consult-find-sort-by-depth-rg-fast "Quick files Search" :exit t)
     ("f" (if my-find-current-window
           (my/consult-find-sort-by-depth-rg-fast-select)
           (my/find-file-split-right))
      (format "Find file %s" (if my-find-current-window "[↓ Here]" "[→ Split]")) :exit t)
     ;; Hidden toggle command
     ("r" consult-recent-file "Recent files")
     ("b" consult-buffer "Buffers")
   )
   "Window"
   (("v" my-split-window-right "Split vertical" :exit t)
    ("-" my-split-window-below "Split Horiz")
    ("d" delete-window "Delete")
    ("j" windmove-left "Move left" :exit t)
    (";" windmove-right "Move right" :exit t)
    ("m" delete-other-windows "Delete Others")
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
    )
   
  ))
  
;; ==================== Goto Menu **

(defhydra hydra-goto (:hint nil :color blue)
  ("w" ace-window :exit t)
  ("SPC" consult-buffer :exit t)) 


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

(defhydra hydra-insert-mode (:color blue :body-pre (insert ".") :idle 1.0 :timeout 0.5)
  ("f" (progn
	 (delete-char -1)
	 (forward-char)
	 ))

  ("w" (progn
	 (delete-char -1)
	 (boon-open-line-and-insert)
	 ))

  ("s" (progn
	 (delete-char -1)
	 (boon-open-next-line-and-insert)
	 ))
  )


;; ========================================================================== EOF ============================================================
(provide 'pretty-hydras)
