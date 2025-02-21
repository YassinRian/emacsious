;;; search-help.el --- Documentation for search commands -*- lexical-binding: t -*-

;;; Commentary:
;; Provides documentation and access to various search commands

;;; Code:

(defun my/show-search-help ()
  "Display help buffer for search commands."
  (interactive)
  (with-current-buffer (get-buffer-create "*Search Commands Help*")
    (erase-buffer)
    (org-mode)
    (insert "* Search Commands Help
This guide explains all available search commands and their usage.

** Project Searches
- =consult-ripgrep= :: Searches text in all project files using ripgrep
- =deadgrep= :: Interactive UI for ripgrep searches
- =consult-find= :: Find files by name in project
- =affe-find= :: Fuzzy file finding with approximate matching

** Buffer Searches
- =consult-line= :: Search in current buffer (like Ctrl-F)
- =consult-buffer= :: Quick buffer switching with search
- =consult-line-multi= :: Search across all open buffers
- =Other Window Search= :: Search in buffer clone in new window

** Advanced Searches
- =consult-imenu= :: Jump to code structures in current buffer
- =consult-imenu-multi= :: Jump to code structures across buffers
- =consult-outline= :: Navigate between headings
- =consult-goto-line= :: Go to specific line number

** Other Searches
- =Directory Search= :: Find files in specific directory
- =consult-recent-file= :: Browse and search recent files
- =consult-mark= :: Jump between buffer marks
- =consult-git-grep= :: Search in git-tracked files

** Usage Tips
1. Press '?' or 'h' in any search interface for more help
2. Most searches support regexp patterns
3. Use C-h f on any command name for detailed docs

** Quick Access
You can access the search hydra with the key binding you've set
or M-x hydra-search/body

Press 'q' to close this help buffer.")
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (setq buffer-read-only t)
    (local-set-key (kbd "q") 'quit-window)))

;; Bind both the help and the hydra
(global-set-key (kbd "C-c s ?") 'my/show-search-help)  ; Help
(global-set-key (kbd "C-c s s") 'hydra-search/body)    ; Search Hydra

(provide 'search-help)
;;; search-help.el ends here
