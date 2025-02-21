;;; config-bookmarks.el --- Advanced bookmark configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive bookmark functionality using Hydra

;;; Code:

(require 'hydra)
(require 'bookmark)

;; ========================= Bookmark Hydra ================================

(defhydra hydra-bookmarks (:hint nil :exit t)
  "
  Bookmark Commands
  ^Basic^             ^Jump^              ^Edit^              ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _s_: Set           _j_: Jump           _r_: Rename         _l_: List
  _m_: Set Named     _f_: Jump Other     _d_: Delete        _i_: Insert
  _b_: Set File      _p_: Previous       _a_: Annotate      _L_: Load
  _S_: Save          _n_: Next           _x_: Delete All    _w_: Write
  
  "
  ;; Basic bookmark operations
  ("s" bookmark-set "set bookmark")
  ("m" bookmark-set-no-overwrite "set named")
  ("b" bookmark-set-filename "set file")
  ("S" bookmark-save "save bookmarks")
  
  ;; Jump operations
  ("j" bookmark-jump "jump to bookmark")
  ("f" bookmark-jump-other-window "jump other window")
  ("p" bookmark-previous "previous bookmark")
  ("n" bookmark-next "next bookmark")
  
  ;; Edit operations
  ("r" bookmark-rename "rename bookmark")
  ("d" bookmark-delete "delete bookmark")
  ("a" bookmark-edit-annotation "edit annotation")
  ("x" bookmark-delete-all "delete all")
  
  ;; Special operations
  ("l" list-bookmarks "list bookmarks")
  ("i" bookmark-insert "insert location")
  ("L" bookmark-load "load bookmarks")
  ("w" bookmark-write "write bookmarks")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c B") #'hydra-bookmarks/body)

;; ========================= Bookmark Configuration =====================

;; Basic bookmark settings
(setq bookmark-default-file "~/.emacs.d/bookmarks"  ; Bookmark file location
      bookmark-save-flag 1                          ; Save after each change
      bookmark-version-control t)                   ; Use version control

;; Bookmark display settings
(setq bookmark-bmenu-file-column 35                ; Width of filename column
      bookmark-bmenu-toggle-filenames t            ; Show filenames by default
      bookmark-search-size 100)                    ; Search size for finding position

;; Auto-save bookmarks when Emacs is idle
(run-with-idle-timer 30 t #'bookmark-save)

;; Helper functions for bookmark management
(defun my/bookmark-next-window ()
  "Jump to next bookmark in another window."
  (interactive)
  (let ((bookmark (bookmark-next-bookmark)))
    (when bookmark
      (bookmark-jump-other-window bookmark))))

(defun my/bookmark-set-project ()
  "Set a bookmark for the current project root."
  (interactive)
  (when-let ((project-root (project-root (project-current t))))
    (bookmark-set (format "Project: %s" 
                         (file-name-nondirectory 
                          (directory-file-name project-root))))))

;; Additional bookmark functions
(defun my/bookmark-to-register (bookmark)
  "Convert BOOKMARK to a register for quick access."
  (interactive (list (bookmark-completing-read "Bookmark to register"
                                             (bookmark-all-names))))
  (let ((reg (read-char "Register to store bookmark in: ")))
    (set-register reg `(bookmark . ,bookmark))
    (message "Stored bookmark %s in register %c" bookmark reg)))

;; Bookmark filtering and organization
(defun my/bookmark-by-type ()
  "Display bookmarks grouped by their type."
  (interactive)
  (let ((types (make-hash-table :test 'equal)))
    (dolist (bookmark (bookmark-all-names))
      (let* ((record (bookmark-get-bookmark bookmark))
             (filename (bookmark-get-filename record))
             (type (cond
                    ((not filename) "No File")
                    ((file-directory-p filename) "Directory")
                    (t (file-name-extension filename "No Extension")))))
        (push bookmark (gethash type types))))
    (with-current-buffer (get-buffer-create "*Bookmark Types*")
      (erase-buffer)
      (maphash (lambda (type bookmarks)
                 (insert (format "\n=== %s ===\n" type))
                 (dolist (bookmark (nreverse bookmarks))
                   (insert (format "  %s\n" bookmark))))
               types)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'config-bookmarks)
;;; config-bookmarks.el ends here
