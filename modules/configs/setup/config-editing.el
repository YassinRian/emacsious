;;; config-editing.el --- Advanced editing configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive editing functionality using Hydra

;;; Code:

(require 'hydra)
(require 'expand-region)
(require 'multiple-cursors)
(require 'avy)

;; ========================= Editing Hydra ================================

(defhydra hydra-editing (:hint nil :exit t)
  "
  Editing Commands
  ^Basic^            ^Advanced^         ^Multiple^         ^Move^
  ^^^^^^^^-----------------------------------------------------------------
  _u_: Undo         _e_: Expand        _c_: Create MC     _j_: Jump Char
  _r_: Redo         _s_: Contract      _l_: Lines         _w_: Jump Word
  _y_: Yank Pop     _t_: Transpose     _n_: Numbers       _g_: Jump Line
  _f_: Fill         _b_: Blank Lines   _a_: All Like This _m_: Mark Ring
  
  ^Format^          ^Lines^            ^Case^             ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _i_: Indent       _d_: Duplicate     _U_: Upcase        _z_: Zap to Char
  _I_: Auto-indent  _k_: Kill          _L_: Lowercase     _x_: Delete Blank
  _T_: Tabs Mode    _J_: Join          _C_: Capitalize    _v_: Delete Space
  _W_: Whitespace   _S_: Sort          _R_: Replace       _p_: Delete Para
  "
  ;; Basic Operations
  ("u" undo "undo")
  ("r" undo-redo "redo")
  ("y" yank-pop "yank pop")
  ("f" fill-paragraph "fill")
  
  ;; Advanced Operations
  ("e" er/expand-region "expand")
  ("s" er/contract-region "contract")
  ("t" transpose-words "transpose")
  ("b" delete-blank-lines "blank lines")
  
  ;; Multiple Cursors
  ("c" mc/edit-lines "create mc")
  ("l" mc/edit-ends-of-lines "lines")
  ("n" mc/insert-numbers "numbers")
  ("a" mc/mark-all-like-this "all like this")
  
  ;; Movement
  ("j" avy-goto-char "jump char")
  ("w" avy-goto-word-1 "jump word")
  ("g" avy-goto-line "jump line")
  ("m" pop-global-mark "mark ring")
  
  ;; Formatting
  ("i" indent-region "indent")
  ("I" indent-according-to-mode "auto-indent")
  ("T" indent-tabs-mode "tabs mode")
  ("W" whitespace-mode "whitespace")
  
  ;; Line Operations
  ("d" duplicate-line "duplicate")
  ("k" kill-whole-line "kill")
  ("J" join-line "join")
  ("S" sort-lines "sort")
  
  ;; Case Operations
  ("U" upcase-region "upcase")
  ("L" downcase-region "lowercase")
  ("C" capitalize-region "capitalize")
  ("R" replace-string "replace")
  
  ;; Special Operations
  ("z" zap-to-char "zap to char")
  ("x" delete-blank-lines "delete blank")
  ("v" delete-horizontal-space "delete space")
  ("p" delete-paragraph "delete para")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c e") #'hydra-editing/body)

;; ========================= Helper Functions ============================

(defun duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((column (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (newline)
    (yank)
    (move-to-column column)))

;; ========================= Editing Configuration =====================

;; Basic editing settings
(setq-default indent-tabs-mode nil           ; Use spaces instead of tabs
              tab-width 4                    ; Set tab width
              require-final-newline t        ; Add newline at end of files
              sentence-end-double-space nil) ; Single space after sentences

;; Delete selection mode
(delete-selection-mode 1)

;; Electric pair mode
(electric-pair-mode 1)
(setq electric-pair-preserve-balance t
      electric-pair-skip-self 'electric-pair-default-skip-self
      electric-pair-inhibit-predicate 'electric-pair-default-inhibit)

;; Expand region settings
(setq expand-region-fast-keys-enabled t
      expand-region-subword-enabled t)

;; Multiple cursors settings
(setq mc/always-run-for-all t
      mc/insert-numbers-default 1)

;; Avy settings
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      avy-background t
      avy-style 'at-full)

;; Better unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Paredit for Lisp modes
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook #'paredit-mode)

;; Subword mode for camelCase
(add-hook 'prog-mode-hook #'subword-mode)

;; Show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)

;; Auto fill settings
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Whitespace visualization
(setq whitespace-style '(face tabs spaces trailing lines space-before-tab
                             newline indentation empty space-after-tab
                             space-mark tab-mark newline-mark))

;; Save place in files
(save-place-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-max-menu-items 15)

;; Smart tab behavior
(setq tab-always-indent 'complete)

;; Kill ring settings
(setq kill-ring-max 100
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t)

(provide 'config-editing)
;;; config-editing.el ends here
