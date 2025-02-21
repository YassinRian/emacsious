;;; config-debug.el --- Advanced debugging configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive debugging functionality using Hydra and DAP-mode

;;; Code:

(require 'hydra)
(require 'dap-mode)
(require 'dap-ui)
(require 'dap-python)
(require 'dap-lldb)
(require 'realgud)

;; ========================= Debug Hydra ================================

(defhydra hydra-debug (:hint nil :exit t)
  "
  Debug Commands
  ^Basic^            ^Step^             ^Breakpoints^      ^Info^
  ^^^^^^^^-----------------------------------------------------------------
  _d_: Debug        _n_: Next          _b_: Toggle        _i_: Info
  _r_: Run          _i_: Step In       _B_: List          _l_: Locals
  _c_: Continue     _o_: Step Out      _e_: Edit          _s_: Stack
  _q_: Quit         _h_: Here          _D_: Delete All    _w_: Watch
  
  ^DAP^             ^Sessions^         ^Windows^          ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _p_: Python       _S_: Switch        _1_: Output        _t_: Test
  _j_: Java         _L_: List All      _2_: Locals        _f_: Find Func
  _g_: Go           _k_: Kill          _3_: Expressions   _x_: Execute
  _R_: Rust         _K_: Kill All      _4_: Repl          _v_: Variables
  "
  ;; Basic Debug
  ("d" dap-debug "debug")
  ("r" dap-debug-restart "run")
  ("c" dap-continue "continue")
  ("q" dap-disconnect "quit")
  
  ;; Stepping
  ("n" dap-next "next")
  ("i" dap-step-in "step in")
  ("o" dap-step-out "step out")
  ("h" dap-breakpoint-toggle "break here")
  
  ;; Breakpoints
  ("b" dap-breakpoint-toggle "toggle break")
  ("B" dap-breakpoint-list "list breaks")
  ("e" dap-breakpoint-edit "edit break")
  ("D" dap-breakpoint-delete-all "delete all")
  
  ;; Info Views
  ("i" dap-ui-inspect "inspect")
  ("l" dap-ui-locals "locals")
  ("s" dap-ui-stack-frames "stack")
  ("w" dap-ui-watch "watch")
  
  ;; DAP Modes
  ("p" dap-python-debug "python")
  ("j" dap-java-debug "java")
  ("g" dap-go-debug "go")
  ("R" dap-lldb-debug "rust")
  
  ;; Sessions
  ("S" dap-switch-session "switch")
  ("L" dap-debug-list-sessions "list all")
  ("k" dap-delete-session "kill")
  ("K" dap-delete-all-sessions "kill all")
  
  ;; Windows
  ("1" dap-ui-output "output")
  ("2" dap-ui-locals "locals")
  ("3" dap-ui-expressions "expressions")
  ("4" dap-ui-repl "repl")
  
  ;; Special
  ("t" dap-debug-test-at-point "test")
  ("f" dap-debug-find-function "find func")
  ("x" dap-debug-eval "execute")
  ("v" dap-ui-variables "variables")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c D") #'hydra-debug/body)

;; ========================= Helper Functions ============================

(defun dap-debug-find-function ()
  "Find and debug a specific function."
  (interactive)
  (let ((func (completing-read "Function to debug: "
                              (list-function-names-in-buffer))))
    (dap-breakpoint-add nil (point))
    (dap-debug)))

(defun list-function-names-in-buffer ()
  "List all function names in current buffer."
  (let ((functions '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*\\(def\\|function\\)\\s-+\\([^ (]+\\)" nil t)
        (push (match-string 2) functions)))
    functions))

;; ========================= Debug Configuration =======================

;; DAP-mode setup
(dap-mode 1)
(dap-ui-mode 1)
(dap-tooltip-mode 1)
(dap-ui-controls-mode 1)

;; Window configuration
(setq dap-auto-configure-features '(sessions locals controls tooltip))

;; Python configuration
(setq dap-python-executable "python3"
      dap-python-debugger 'debugpy)

;; LLDB configuration
(setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))

;; Default debug template
(dap-register-debug-template
 "Python :: Run Configuration"
 (list :type "python"
       :request "launch"
       :name "Python :: Run Configuration"
       :cwd nil
       :program nil))

;; Breakpoint faces
(set-face-attribute 'dap-breakpoint-face nil
                    :foreground "red"
                    :background "dark red")

(set-face-attribute 'dap-stopped-face nil
                    :foreground "white"
                    :background "dark green")

;; Output buffer configuration
(setq dap-output-buffer-width 80
      dap-output-buffer-height 20)

;; RealGUD configuration
(setq realgud:pdb-command-name "python -m pdb"
      realgud:gdb-command-name "gdb -i=mi")

;; Auto-show locals on break
(add-hook 'dap-stopped-hook
          (lambda (arg) 
            (call-interactively #'dap-hydra)))

;; Language-specific debug configurations
(with-eval-after-load 'dap-mode
  ;; Python
  (require 'dap-python)
  (setq dap-python-default-debug-port 5678)
  
  ;; Java
  (require 'dap-java)
  
  ;; Go
  (require 'dap-go)
  (setq dap-go-debug-program '("dlv" "dap"))
  
  ;; LLDB
  (require 'dap-lldb)
  (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode")))

;; Debug key bindings
(with-eval-after-load 'dap-mode
  (define-key dap-mode-map (kbd "<f5>") #'dap-debug)
  (define-key dap-mode-map (kbd "<f6>") #'dap-next)
  (define-key dap-mode-map (kbd "<f7>") #'dap-step-in)
  (define-key dap-mode-map (kbd "<f8>") #'dap-step-out))

(provide 'config-debug)
;;; config-debug.el ends here
