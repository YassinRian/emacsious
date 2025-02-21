;;; config-spelling.el --- Advanced spelling configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive spelling functionality using Hydra
;; Integrates flyspell, ispell, and other spelling tools

;;; Code:

(require 'hydra)
(require 'flyspell)
(require 'ispell)

;; ========================= Spelling Hydra ================================

(defhydra hydra-spelling (:hint nil :exit t)
  "
  Spelling Commands
  ^Check^            ^Correct^          ^Dictionary^       ^Flyspell^
  ^^^^^^^^-----------------------------------------------------------------
  _c_: Check Word   _f_: Fix Word      _d_: Change Dict   _t_: Toggle
  _b_: Check Back   _n_: Next Error    _a_: Add Word      _m_: Mode
  _r_: Check Region _p_: Prev Error    _s_: Save Word     _B_: Buffer
  _w_: Check Wrap   _l_: List Errors   _i_: Import Dict   _P_: Prog Mode
  
  ^Special^          ^Languages^        ^Cache^            ^Tools^
  ^^^^^^^^-----------------------------------------------------------------
  _x_: Extra Words   _e_: English      _C_: Clear Cache   _h_: Hunspell
  _k_: Known Word    _g_: German       _R_: Reload Cache  _A_: Aspell
  _u_: Unknown Word  _F_: French       _S_: Save Cache    _I_: Ispell
  _G_: Guess         _o_: More...      _L_: Load Cache    _M_: Messages
  "
  ;; Check Operations
  ("c" ispell-word "check word")
  ("b" ispell-backward-word "check back")
  ("r" ispell-region "check region")
  ("w" ispell-wrap-region "check wrap")
  
  ;; Correction
  ("f" flyspell-correct-word-before-point "fix word")
  ("n" flyspell-goto-next-error "next error")
  ("p" flyspell-goto-prev-error "prev error")
  ("l" flyspell-list-errors "list errors")
  
  ;; Dictionary Management
  ("d" ispell-change-dictionary "change dict")
  ("a" ispell-word-add "add word")
  ("s" ispell-word-save "save word")
  ("i" ispell-import-dictionary "import dict")
  
  ;; Flyspell Operations
  ("t" flyspell-mode "toggle")
  ("m" flyspell-mode "mode")
  ("B" flyspell-buffer "buffer")
  ("P" flyspell-prog-mode "prog mode")
  
  ;; Special Operations
  ("x" ispell-insert-extra-words "extra words")
  ("k" ispell-kill-word "known word")
  ("u" ispell-kill-local-word "unknown word")
  ("G" ispell-guess "guess")
  
  ;; Language Selection
  ("e" (ispell-change-dictionary "english") "english")
  ("g" (ispell-change-dictionary "german") "german")
  ("F" (ispell-change-dictionary "french") "french")
  ("o" ispell-valid-dictionary-list "more...")
  
  ;; Cache Operations
  ("C" ispell-clear-cache "clear cache")
  ("R" ispell-reload "reload cache")
  ("S" ispell-save-cache "save cache")
  ("L" ispell-load-cache "load cache")
  
  ;; Tools
  ("h" (ispell-change-ispell-program "hunspell") "hunspell")
  ("A" (ispell-change-ispell-program "aspell") "aspell")
  ("I" (ispell-change-ispell-program "ispell") "ispell")
  ("M" (switch-to-buffer "*Messages*") "messages")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c s") #'hydra-spelling/body)

;; ========================= Helper Functions ============================

(defun ispell-wrap-region ()
  "Check spelling of text between point and mark, wrapping around buffer end."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (ispell-region start end))
    (let ((start (point))
          (end (save-excursion
                 (forward-paragraph)
                 (point))))
      (ispell-region start end))))

(defun ispell-guess ()
  "Guess the correct spelling using various dictionaries."
  (interactive)
  (let ((word (thing-at-point 'word t))
        (dicts '("english" "american" "british")))
    (when word
      (dolist (dict dicts)
        (ispell-change-dictionary dict)
        (ispell-word word)))))

;; ========================= Spelling Configuration =====================

;; Basic spelling settings
(setq ispell-program-name (executable-find "aspell")
      ispell-dictionary "english"
      ispell-silently-savep t
      ispell-choices-win-default-height 4
      ispell-following-word t
      ispell-highlight-face 'isearch)

;; Flyspell settings
(setq flyspell-issue-message-flag nil
      flyspell-issue-welcome-flag nil
      flyspell-mark-duplications-flag t
      flyspell-persistent-highlight t
      flyspell-duplicate-distance 12000)

;; Aspell configuration
(when (equal ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra"
                           "--lang=en_US"
                           "--camel-case")))

;; Hunspell configuration
(when (equal ispell-program-name "hunspell")
  (setq ispell-hunspell-dict-paths-alist
        '(("en_US" "/usr/share/hunspell/en_US.aff")
          ("de_DE" "/usr/share/hunspell/de_DE.aff")
          ("fr_FR" "/usr/share/hunspell/fr_FR.aff"))))

;; Dictionary paths
(setq ispell-personal-dictionary "~/.emacs.d/spelling/personal-dict"
      ispell-alternate-dictionary "/usr/share/dict/words")

;; Ensure dictionary directory exists
(make-directory (file-name-directory ispell-personal-dictionary) t)

;; Mode hooks
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Prog-mode spell checking
(add-hook 'prog-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (ispell-change-dictionary "english")))

;; Text-mode spell checking
(add-hook 'text-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (ispell-change-dictionary "english")))

;; Custom skip functions
(add-to-list 'ispell-skip-region-alist '("\\$\\$" . "\\$\\$"))
(add-to-list 'ispell-skip-region-alist '("\\\\(" . "\\\\)"))

;; Performance optimization
(setq flyspell-large-region 1000)
(setq ispell-async-processp t)

(provide 'config-spelling)
;;; config-spelling.el ends here
