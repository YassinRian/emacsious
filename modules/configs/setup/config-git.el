;;; config-git.el --- Advanced git configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive git functionality using Hydra and Magit

;;; Code:

(require 'hydra)
(require 'magit)
(require 'forge)
(require 'git-timemachine)
(require 'diff-hl)

;; ========================= Git Hydra ================================

(defhydra hydra-git (:hint nil :exit t)
  "
  Git Commands
  ^Basic^            ^Branch^           ^Remote^           ^Changes^
  ^^^^^^^^-----------------------------------------------------------------
  _s_: Status       _b_: Branch        _f_: Fetch         _c_: Commit
  _i_: Init         _B_: Checkout      _F_: Pull          _p_: Push
  _l_: Log          _m_: Merge         _P_: Push MR       _d_: Diff
  _t_: Tag          _r_: Rebase        _R_: Remote        _S_: Stage
  
  ^History^          ^Stash^            ^Issues^           ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _h_: Time Machine  _z_: Stash Pop     _I_: Issues       _w_: Browse
  _L_: File Log      _Z_: Stash List    _M_: MR List      _g_: Refresh
  _B_: Blame         _k_: Stash Push    _C_: Comments     _a_: Forge
  _v_: Revert        _K_: Stash Apply   _T_: Todo         _x_: Execute
  "
  ;; Basic Operations
  ("s" magit-status "status")
  ("i" magit-init "init")
  ("l" magit-log-current "log")
  ("t" magit-tag "tag")
  
  ;; Branch Operations
  ("b" magit-branch "branch")
  ("B" magit-checkout "checkout")
  ("m" magit-merge "merge")
  ("r" magit-rebase "rebase")
  
  ;; Remote Operations
  ("f" magit-fetch "fetch")
  ("F" magit-pull "pull")
  ("P" forge-create-pullreq "create PR")
  ("R" magit-remote "remote")
  
  ;; Change Management
  ("c" magit-commit "commit")
  ("p" magit-push "push")
  ("d" magit-diff "diff")
  ("S" magit-stage "stage")
  
  ;; History Navigation
  ("h" git-timemachine "time machine")
  ("L" magit-log-buffer-file "file log")
  ("B" magit-blame "blame")
  ("v" magit-revert "revert")
  
  ;; Stash Management
  ("z" magit-stash-pop "stash pop")
  ("Z" magit-stash-list "stash list")
  ("k" magit-stash-push "stash push")
  ("K" magit-stash-apply "stash apply")
  
  ;; Issue Tracking
  ("I" forge-list-issues "issues")
  ("M" forge-list-pullreqs "merge requests")
  ("C" forge-list-comments "comments")
  ("T" forge-list-labeled-issues "todo")
  
  ;; Special Operations
  ("w" browse-at-remote "browse remote")
  ("g" magit-refresh "refresh")
  ("a" forge-pull "forge sync")
  ("x" magit-run "execute")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c g") #'hydra-git/body)

;; ========================= Helper Functions ============================

(defun magit-run ()
  "Run a git command directly."
  (interactive)
  (let ((command (read-string "Git command: ")))
    (magit-run-git-with-editor (split-string command))))

(defun git-create-ignore ()
  "Create a .gitignore file in the current project."
  (interactive)
  (let ((file (concat (projectile-project-root) ".gitignore")))
    (find-file file)
    (unless (file-exists-p file)
      (insert "# Generated .gitignore file\n\n"))))

;; ========================= Git Configuration =======================

;; Magit settings
(setq magit-repository-directories '(("~/Projects" . 2))
      magit-save-repository-buffers 'dontask
      magit-no-confirm '(stage-all-changes
                        unstage-all-changes)
      magit-diff-refine-hunk t
      magit-status-show-hashes-in-headers t)

;; Forge settings
(setq forge-database-file (expand-file-name "forge-database.sqlite" user-emacs-directory)
      forge-topic-list-limit '(60 . 0))

;; Git-timemachine settings
(setq git-timemachine-show-minibuffer-details t)

;; Diff-hl settings
(global-diff-hl-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Git commit message settings
(setq git-commit-summary-max-length 50
      git-commit-fill-column 72
      git-commit-style-convention-checks '(overlong-summary-line
                                         non-empty-second-line))

;; Auto-revert settings for git
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Git related hooks
(add-hook 'git-commit-mode-hook #'turn-on-flyspell)
(add-hook 'magit-mode-hook #'magit-auto-revert-mode)

;; Custom git face settings
(with-eval-after-load 'magit
  (set-face-foreground 'magit-branch-remote "orange")
  (set-face-foreground 'magit-branch-local "green")
  (set-face-foreground 'magit-section-heading "blue"))

;; Git blame format
(setq magit-blame-heading-format "%-20a %C %s"
      magit-blame-time-format "%Y-%m-%d")

;; Forge authentication
(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("gitlab.company.com" "gitlab.company.com/api/v4"
                 "gitlab.company.com" forge-gitlab-repository)))

;; Git URL rewrites
(with-eval-after-load 'magit
  (add-to-list 'magit-repository-directories
               '("~/Work" . 2))
  (add-to-list 'magit-clone-name-alist
               '("^github.com" ":user/:name")))

(provide 'config-git)
;;; config-git.el ends here
