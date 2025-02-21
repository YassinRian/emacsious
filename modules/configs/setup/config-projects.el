;;; config-projects.el --- Project management configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive project functionality using Hydra and project.el

;;; Code:

(require 'hydra)
(require 'project)

;; ========================= Projects Hydra ================================

(defhydra hydra-projects (:hint nil :exit t)
  "
  Project Commands
  ^Navigation^        ^Search^           ^Buffers^          ^Files^
  ^^^^^^^^-----------------------------------------------------------------
  _p_: Switch        _s_: Search        _b_: Buffers       _f_: Find File
  _d_: Dired         _r_: Replace       _k_: Kill Bufs     _a_: Add File
  _v_: VC-Dir        _g_: Grep          _B_: List Bufs     _D_: Delete
  _c_: Compile       _t_: Tags          _S_: Save All      _R_: Recent

  ^Shell^            ^External^         ^Special^          ^Info^
  ^^^^^^^^-----------------------------------------------------------------
  _$_: Shell         _e_: Eshell       _x_: Execute       _i_: Project Info
  _m_: Make          _&_: Async         _!_: Shell Cmd     _w_: Where Is
  _C_: Configure     _V_: VTERM         _z_: Cache         _h_: History
  _F_: Find Dir      _'_: Term          _E_: Edit Config   _I_: IDE View
  "
  ;; Navigation
  ("p" project-switch-project "switch project")
  ("d" project-dired "dired")
  ("v" project-vc-dir "vc-dir")
  ("c" project-compile "compile")
  
  ;; Search
  ("s" project-search "search")
  ("r" project-query-replace-regexp "replace")
  ("g" project-find-regexp "grep")
  ("t" project-tags "tags")
  
  ;; Buffers
  ("b" project-switch-to-buffer "switch buffer")
  ("k" project-kill-buffers "kill buffers")
  ("B" project-list-buffers "list buffers")
  ("S" project-save-buffers "save all")
  
  ;; Files
  ("f" project-find-file "find file")
  ("a" project-add-file "add file")
  ("D" project-remove-file "delete")
  ("R" project-find-recent-file "recent")
  
  ;; Shell
  ("$" project-shell "shell")
  ("m" project-make "make")
  ("C" project-configure "configure")
  ("F" project-find-dir "find directory")
  
  ;; External
  ("e" project-eshell "eshell")
  ("&" project-async-shell-command "async command")
  ("V" project-vterm "vterm")
  ("'" project-term "terminal")
  
  ;; Special
  ("x" project-execute-extended-command "execute")
  ("!" project-shell-command "shell command")
  ("z" project-forget-project "clear cache")
  ("E" (find-file (project-root (project-current))) "edit config")
  
  ;; Info
  ("i" project-info "info")
  ("w" project-where-is "where is")
  ("h" project-history "history")
  ("I" project-ide-view "IDE view")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c P") #'hydra-projects/body)

;; ========================= Helper Functions ============================

(defun project-save-buffers ()
  "Save all buffers belonging to current project."
  (interactive)
  (let* ((pr (project-current t))
         (buffers (project-buffers pr)))
    (dolist (buf buffers)
      (with-current-buffer buf
        (when (buffer-modified-p)
          (save-buffer))))
    (message "Saved all project buffers")))

(defun project-ide-view ()
  "Setup an IDE-like view for the current project."
  (interactive)
  (delete-other-windows)
  ;; Left side - project tree
  (let ((left-window (selected-window)))
    (project-dired)
    ;; Main editing area
    (select-window left-window)
    (split-window-right 30)
    (other-window 1)
    ;; Bottom area for compilation/shell
    (split-window-below 75)
    (other-window 1)
    (project-eshell)))

(defun project-add-file ()
  "Create a new file in the current project."
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (file (read-file-name "Create file: " root)))
    (find-file file)))

(defun project-find-recent-file ()
  "Find recent file in current project."
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (recent-files
          (seq-filter
           (lambda (file)
             (string-prefix-p root (expand-file-name file)))
           recentf-list)))
    (if recent-files
        (find-file
         (completing-read "Recent project files: "
                         recent-files nil t))
      (message "No recent files in project"))))

;; ========================= Project Configuration ======================

;; Basic project settings
(setq project-list-file (locate-user-emacs-file "projects")
      project-switch-commands 'project-find-file
      project-vc-extra-root-markers '(".project" "package.json" "Cargo.toml"))

;; Project compilation commands
(setq project-compilation-commands
      '((?c "check" . "cargo check")
        (?t "test" . "cargo test")
        (?r "run" . "cargo run")))

;; Project ignores
(setq project-ignore-patterns
      '(".git" ".svn" "node_modules" "target" "dist" "build"))

;; Auto-refresh project files
(setq project-file-watch-delay 0.5)

;; Project history
(setq project-history-max-length 50)

;; Project shell configuration
(setq project-shell-command-history-variable 'project-shell-command-history)

(provide 'config-projects)
;;; config-projects.el ends here
