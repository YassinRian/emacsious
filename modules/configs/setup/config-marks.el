;; Mark management functions and pretty-hydra configuration

;; ----------------------------------------------------------
;; Mark Navigation Functions
;; ----------------------------------------------------------

;; Make sure we have prerequisites
(require 'pretty-hydra)
(require 'consult)


(defun my/consult-mark-forward ()
  "Jump to the next mark in the local mark ring."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'set-mark-command)))

(defun my/consult-mark-backward ()
  "Jump to the previous mark in the local mark ring."
  (interactive)
  (let ((current-prefix-arg '(-4)))  ;; Negative prefix to go backward
    (call-interactively 'set-mark-command)))

(defun my/push-mark-with-feedback ()
  "Push mark at point and display a message."
  (interactive)
  (push-mark nil t)
  (message "Mark set (position %s)" (point)))

(defun my/swap-point-and-mark ()
  "Swap point and mark without activating the region."
  (interactive)
  (let ((mark-active nil))
    (exchange-point-and-mark)))

(defun my/exchange-point-and-mark ()
  "Swap point and mark without activating the region."
  (interactive)
  (exchange-point-and-mark)
    (my-modal-enter-visual-mode))

(defun my/mark-whole-buffer-keep-position ()
  "Mark the whole buffer and return to original position."
  (interactive)
  (let ((current-point (point)))
    (push-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (goto-char current-point)
    (my-modal-enter-visual-mode)))

;; Register-based mark functions
(defun my/quick-mark-set (arg)
  "Set a mark in register 0-9."
  (interactive "cSet mark in register (0-9): ")
  (point-to-register arg)
  (message "Mark set in register '%c'" arg))

(defun my/quick-mark-jump (arg)
  "Jump to mark in register 0-9."
  (interactive "cJump to register (0-9): ")
  (jump-to-register arg))

;; Enable automatic mark setting for major movements
(defun my/enable-auto-mark-for-navigation ()
  "Set up automatic marks before major navigation commands."
  (interactive)
  (advice-add 'isearch-forward :before #'push-mark)
  (advice-add 'isearch-backward :before #'push-mark)
  (advice-add 'beginning-of-buffer :before #'push-mark)
  (advice-add 'end-of-buffer :before #'push-mark)
  (advice-add 'imenu :before #'push-mark)
  (message "Automatic mark setting enabled for navigation commands"))

(defun my/disable-auto-mark-for-navigation ()
  "Remove automatic marks before major navigation commands."
  (interactive)
  (advice-remove 'isearch-forward #'push-mark)
  (advice-remove 'isearch-backward #'push-mark)
  (advice-remove 'beginning-of-buffer #'push-mark)
  (advice-remove 'end-of-buffer #'push-mark)
  (advice-remove 'imenu #'push-mark)
  (message "Automatic mark setting disabled for navigation commands"))

;; Additional useful mark functions
(defun my/copy-region-to-register (arg)
  "Copy region to a register 0-9."
  (interactive "cCopy to register (0-9): ")
  (if (region-active-p)
      (progn
        (copy-to-register arg (region-beginning) (region-end))
        (message "Region copied to register '%c'" arg))
    (message "No active region")))

(defun my/paste-from-register (arg)
  "Insert text from register 0-9."
  (interactive "cInsert from register (0-9): ")
  (insert-register arg)
  (message "Inserted from register '%c'" arg))

(defun my/show-mark-ring ()
  "Show the contents of the mark ring."
  (interactive)
  (with-current-buffer (get-buffer-create "*Mark Ring*")
    (erase-buffer)
    (insert "Local Mark Ring:\n\n")
    (let ((count 0)
          (the-mark-ring (cons (mark-marker) mark-ring)))
      (dolist (mark the-mark-ring)
        (when (marker-position mark)
          (let ((line (with-current-buffer (marker-buffer mark)
                        (save-excursion
                          (goto-char mark)
                          (let ((line (line-number-at-pos))
                                (text (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position))))
                            (format "Line %d: %s" line text))))))
            (insert (format "[%d] Position %d: %s\n" count (marker-position mark) line))
            (setq count (1+ count))))))
    (insert "\nGlobal Mark Ring:\n\n")
    (let ((count 0))
      (dolist (mark global-mark-ring)
        (when (marker-position mark)
          (let ((buffer (marker-buffer mark))
                (pos (marker-position mark)))
            (when (buffer-live-p buffer)
              (let ((line (with-current-buffer buffer
                            (save-excursion
                              (goto-char pos)
                              (let ((line (line-number-at-pos))
                                    (text (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position))))
                                (format "Line %d: %s" line text))))))
                (insert (format "[%d] Buffer: %s, Position %d: %s\n" 
                                count (buffer-name buffer) pos line))
                (setq count (1+ count))))))))
    (goto-char (point-min))
    (special-mode)
    (display-buffer (current-buffer))))

;; ----------------------------------------------------------
;; Pretty Hydra Configuration
;; ----------------------------------------------------------

(require 'pretty-hydra)

(pretty-hydra-define mark-management-hydra (:title "Mark Management" 
                                           :quit-key "q"
                                           :color blue
                                           :exit t)
  ("Navigation"
   (("f" my/consult-mark-forward "Forward Mark")
    ("b" my/consult-mark-backward "Backward Mark")
    ("s" my/swap-point-and-mark "Swap Point & Mark")
    ("g" pop-global-mark "Global Mark")
    ("x" my/exchange-point-and-mark "Exchange (with region)"))
   
   "Set Marks"
   (("SPC" my/push-mark-with-feedback "Set Mark Here")
    ("a" my/mark-whole-buffer-keep-position "Mark Buffer")
    ("." my/quick-mark-set "Set Register Mark")
    ("," my/quick-mark-jump "Jump to Register Mark")
    ("r" consult-register "Browse Registers"))
   
   "Consult"
   (("m" consult-mark "Consult Local Marks")
    ("M" consult-global-mark "Consult Global Marks")
    ("l" my/show-mark-ring "List All Marks")
    ("h" consult-register-load "Load Register")
    ("w" consult-register-store "Store Register"))
   
   "Registers"
   (("c" my/copy-region-to-register "Copy to Register")
    ("v" my/paste-from-register "Paste from Register")
    ("p" point-to-register "Point to Register")
    ("j" jump-to-register "Jump to Register"))
   
   "Auto Marks"
   (("e" my/enable-auto-mark-for-navigation "Enable Auto Marks")
    ("d" my/disable-auto-mark-for-navigation "Disable Auto Marks"))))

(global-set-key (kbd "C-c C-m") 'mark-management-hydra/body)



;; Load the configuration
(provide 'config-marks)
