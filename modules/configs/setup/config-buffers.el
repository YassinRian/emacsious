;;; config-buffers.el --- Advanced buffer configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive buffer functionality using Pretty-Hydra with ugrep integration
;; Uses the command-line ugrep tool without requiring the Emacs package

;;; Code:

(require 'pretty-hydra)

;; Main hydra for windows, files, and buffers
(pretty-hydra-define my-main-hydra
  (:title "Main Menu" :quit-key "q" :color pink)
  ("Windows"
   (("v" split-window-right "Split vertical")
    ("h" split-window-below "Split horizontal")
    ("d" delete-window "Delete window")
    ("o" delete-other-windows "Delete other windows")
    ("b" balance-windows "Balance windows")
    ("w" my-window-hydra/body "More window commands..." :exit t))
   
   "Files"
   (("f" find-file "Find file")
    ("s" save-buffer "Save file")
    ("r" consult-recent-file "Recent files")
    ("p" project-find-file "Project file")
    ("F" my-file-hydra/body "More file commands..." :exit t))
   
   "Buffers"
   (("k" kill-buffer "Kill buffer")
    ("n" next-buffer "Next buffer")
    ("p" previous-buffer "Previous buffer")
    ("l" consult-buffer "List buffers")
    ("B" my-buffer-hydra/body "More buffer commands..." :exit t))
   
   "Other"
   (("/" my-search-hydra/body "Search menu" :exit t)
    ("q" nil "Quit"))))

;; Expanded window management hydra
(pretty-hydra-define my-window-hydra
  (:title "Window Management" :quit-key "q" :color pink)
  ("Navigate"
   (("h" windmove-left "← Left")
    ("j" windmove-down "↓ Down")
    ("k" windmove-up "↑ Up")
    ("l" windmove-right "→ Right"))
   
   "Resize"
   (("H" shrink-window-horizontally "Shrink horizontally")
    ("J" enlarge-window "Enlarge vertically")
    ("K" shrink-window "Shrink vertically")
    ("L" enlarge-window-horizontally "Enlarge horizontally")
    ("=" balance-windows "Balance all"))
   
   "Actions"
   (("s" ace-swap-window "Swap windows")
    ("m" ace-maximize-window "Maximize window")
    ("u" winner-undo "Undo window change")
    ("r" winner-redo "Redo window change"))
   
   "Exit"
   (("b" my-main-hydra/body "Back to main menu" :exit t)
    ("q" nil "Quit" :exit t))))

;; Expanded file management hydra
(pretty-hydra-define my-file-hydra
  (:title "File Management" :quit-key "q" :color pink)
  ("Navigate"
   (("f" find-file "Find file")
    ("r" consult-recent-file "Recent files")
    ("b" bookmark-jump "Jump to bookmark")
    ("d" dired "Open directory")
    ("p" project-find-file "Find in project"))
   
   "Actions"
   (("s" save-buffer "Save file")
    ("S" write-file "Save as...")
    ("R" rename-file "Rename file")
    ("D" delete-file "Delete file")
    ("c" copy-file "Copy file")
    ("m" bookmark-set "Set bookmark"))
   
   "Project"
   (("P" project-switch-project "Switch project")
    ("g" consult-ripgrep "Grep in project")
    ("G" vc-git-grep "Git grep"))
   
   "Exit"
   (("b" my-main-hydra/body "Back to main menu" :exit t)
    ("q" nil "Quit" :exit t))))

;; Expanded buffer management hydra
(pretty-hydra-define my-buffer-hydra
  (:title "Buffer Management" :quit-key "q" :color pink)
  ("Navigate"
   (("n" next-buffer "Next buffer")
    ("p" previous-buffer "Previous buffer")
    ("l" consult-buffer "List buffers")
    ("b" switch-to-buffer "Switch buffer")
    ("r" revert-buffer "Reload buffer"))
   
   "Actions"
   (("k" kill-buffer "Kill buffer")
    ("K" kill-buffer-and-window "Kill buffer & window")
    ("s" save-buffer "Save buffer")
    ("S" save-some-buffers "Save multiple buffers")
    ("R" rename-buffer "Rename buffer"))
   
   "Special"
   (("z" bury-buffer "Bury buffer")
    ("M" buffer-menu "Buffer menu")
    ("d" diff-buffer-with-file "Diff with file")
    ("e" eval-buffer "Evaluate buffer"))
   
   "Exit"
   (("b" my-main-hydra/body "Back to main menu" :exit t)
    ("q" nil "Quit" :exit t))))

;; Buffer naming
(setq uniquify-buffer-name-style 'forward ; Unique buffer names with path
      uniquify-separator "/"              ; Separator in buffer names
      uniquify-after-kill-buffer-p t      ; Update unique names after killing
      uniquify-ignore-buffers-re "^\\*")  ; Ignore special buffers

(provide 'config-buffers)
;;; config-buffers.el ends here
