;;; config-org.el --- Advanced org-mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive org-mode functionality using Hydra

;;; Code:

(require 'hydra)
(require 'org)
(require 'org-capture)
(require 'org-agenda)

;; ========================= Org Hydra ================================

(defhydra hydra-org (:hint nil :exit t)
  "
  Org Commands
  ^Basic^            ^Structure^        ^Links^            ^Time^
  ^^^^^^^^-----------------------------------------------------------------
  _t_: Todo         _p_: Promote       _l_: Store Link    _s_: Schedule
  _T_: Todo Tree    _d_: Demote        _i_: Insert Link   _d_: Deadline
  _m_: Mark Done    _u_: Up Level      _f_: Follow Link   _._: Timestamp
  _r_: Refile      _n_: Down Level     _L_: List Links    _c_: Clock In
  
  ^Capture^         ^View^             ^Navigation^       ^Export^
  ^^^^^^^^-----------------------------------------------------------------
  _C_: Capture      _v_: Overview      _b_: Backward      _e_: Export
  _a_: Agenda       _V_: Global View   _w_: Forward       _E_: Exp. Buffer
  _A_: Archives     _S_: Show All      _j_: Jump To       _h_: HTML
  _P_: Properties   _H_: Hide All      _g_: Goto         _p_: PDF
  "
  ;; Basic
  ("t" org-todo "todo")
  ("T" org-show-todo-tree "todo tree")
  ("m" org-toggle-checkbox "mark done")
  ("r" org-refile "refile")
  
  ;; Structure
  ("p" org-promote-subtree "promote")
  ("d" org-demote-subtree "demote")
  ("u" org-up-element "up level")
  ("n" org-down-element "down level")
  
  ;; Links
  ("l" org-store-link "store link")
  ("i" org-insert-link "insert link")
  ("f" org-open-at-point "follow link")
  ("L" org-insert-all-links "list links")
  
  ;; Time Management
  ("s" org-schedule "schedule")
  ("d" org-deadline "deadline")
  ("." org-time-stamp "timestamp")
  ("c" org-clock-in "clock in")
  
  ;; Capture and Agenda
  ("C" org-capture "capture")
  ("a" org-agenda "agenda")
  ("A" org-toggle-archive-tag "archives")
  ("P" org-set-property "properties")
  
  ;; View Options
  ("v" org-overview "overview")
  ("V" org-global-cycle "global view")
  ("S" org-show-all "show all")
  ("H" org-hide-all "hide all")
  
  ;; Navigation
  ("b" org-backward-heading-same-level "backward")
  ("w" org-forward-heading-same-level "forward")
  ("j" org-goto "jump to")
  ("g" org-agenda-goto "goto")
  
  ;; Export
  ("e" org-export-dispatch "export menu")
  ("E" org-export-buffer "export buffer")
  ("h" org-html-export-to-html "to html")
  ("p" org-latex-export-to-pdf "to pdf")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c o") #'hydra-org/body)

;; ========================= Helper Functions ============================

(defun org-insert-all-links ()
  "Insert all stored links with descriptions."
  (interactive)
  (let ((links (org-link-get-stored-links)))
    (dolist (link links)
      (insert (format "- %s :: %s\n"
                     (car link)
                     (or (cadr link) ""))))))

;; ========================= Org Configuration =======================

;; Basic org settings
(setq org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-archive-location (concat org-directory "/archive.org::* From %s")
      org-log-done 'time
      org-log-into-drawer t
      org-pretty-entities t
      org-startup-indented t
      org-startup-folded 'content
      org-use-fast-todo-selection t)

;; Todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Todo faces
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("NEXT" . (:foreground "blue" :weight bold))
        ("WAITING" . (:foreground "orange" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELLED" . (:foreground "gray" :weight bold))))

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+datetree org-default-notes-file)
         "* %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; Agenda settings
(setq org-agenda-files (list org-directory)
      org-agenda-window-setup 'current-window
      org-agenda-span 'day
      org-agenda-start-on-weekday 1
      org-agenda-show-future-repeats nil)

;; Refile settings
(setq org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)

;; Export settings
(setq org-export-with-toc t
      org-export-with-section-numbers t
      org-export-with-timestamps t
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil)

;; Clock settings
(setq org-clock-persist 'history
      org-clock-persist-query-resume nil
      org-clock-in-resume t
      org-clock-report-include-clocking-task t)

;; Tags
(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("@errands" . ?e)
                      ("@computer" . ?c)
                      ("@phone" . ?p)))

;; Enable org-clock persistence
(org-clock-persistence-insinuate)

;; Load additional org modules
(with-eval-after-load 'org
  (require 'org-habit)
  (require 'org-tempo)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-tempo))

(provide 'config-org)
;;; config-org.el ends here
