;;; config-mail.el --- Advanced mail configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive mail functionality using Hydra and mu4e

;;; Code:

(require 'hydra)
(require 'mu4e)
(require 'org-msg)

;; ========================= Mail Hydra ================================

(defhydra hydra-mail (:hint nil :exit t)
  "
  Mail Commands
  ^Basic^            ^Compose^          ^Search^           ^View^
  ^^^^^^^^-----------------------------------------------------------------
  _u_: Update       _c_: Compose       _s_: Search        _v_: View
  _i_: Index        _r_: Reply         _/_: Narrow        _a_: Attachments
  _m_: Move         _f_: Forward       _b_: Bookmarks     _t_: Thread
  _d_: Delete       _e_: Edit          _l_: Limit         _T_: Toggle Thread
  
  ^Mark^            ^Folders^          ^Actions^          ^Special^
  ^^^^^^^^-----------------------------------------------------------------
  _*_: Mark        _I_: Inbox         _w_: Write         _h_: HTML
  _U_: Unmark      _S_: Sent          _g_: Refresh       _j_: Jump
  _D_: Delete      _A_: Archive       _x_: Execute       _o_: Options
  _F_: Flag        _M_: All Mail      _z_: Sync         _C_: Context
  "
  ;; Basic Operations
  ("u" mu4e-update-mail-and-index "update")
  ("i" mu4e-update-index "index")
  ("m" mu4e-marks-action "move")
  ("d" mu4e-mark-for-delete "delete")
  
  ;; Compose
  ("c" mu4e-compose-new "compose")
  ("r" mu4e-compose-reply "reply")
  ("f" mu4e-compose-forward "forward")
  ("e" mu4e-compose-edit "edit")
  
  ;; Search
  ("s" mu4e-search "search")
  ("/" mu4e-search-narrow "narrow")
  ("b" mu4e-search-bookmark "bookmarks")
  ("l" mu4e-search-limit "limit")
  
  ;; View
  ("v" mu4e-view-message "view")
  ("a" mu4e-view-attachments "attachments")
  ("t" mu4e-view-thread "thread")
  ("T" mu4e-headers-toggle-threading "toggle thread")
  
  ;; Mark Operations
  ("*" mu4e-mark-execute "mark")
  ("U" mu4e-mark-unmark-all "unmark")
  ("D" mu4e-mark-for-delete "mark delete")
  ("F" mu4e-mark-for-flag "flag")
  
  ;; Folders
  ("I" (mu4e-headers-search "maildir:/Inbox") "inbox")
  ("S" (mu4e-headers-search "maildir:/Sent") "sent")
  ("A" (mu4e-headers-search "maildir:/Archive") "archive")
  ("M" (mu4e-headers-search "maildir:/") "all mail")
  
  ;; Actions
  ("w" mu4e-compose-new "write")
  ("g" mu4e-refresh "refresh")
  ("x" mu4e-mark-execute-all "execute")
  ("z" mu4e-update-mail-and-index "sync")
  
  ;; Special
  ("h" org-msg-mode "html")
  ("j" mu4e-jump-to-maildir "jump")
  ("o" mu4e-context-switch "options")
  ("C" mu4e-context-switch "context")
  
  ;; Exit
  ("q" nil "quit" :exit t)
  ("<escape>" nil nil :exit t))

;; Bind the hydra to your preferred key
(global-set-key (kbd "C-c m") #'hydra-mail/body)

;; ========================= Helper Functions ============================

(defun mu4e-search-limit ()
  "Limit search results interactively."
  (interactive)
  (let ((limit (read-number "Limit results to: " mu4e-search-results-limit)))
    (setq mu4e-search-results-limit limit)
    (mu4e-search-rerun)))

(defun mu4e-mark-delete-no-confirm ()
  "Mark message for deletion without confirmation."
  (interactive)
  (mu4e-mark-set 'delete)
  (mu4e-headers-next))

;; ========================= Mail Configuration =======================

;; Basic mu4e settings
(setq mu4e-maildir "~/Mail"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300
      mu4e-attachment-dir "~/Downloads"
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-compose-signature-auto-include t
      mu4e-compose-format-flowed t
      mu4e-headers-skip-duplicates t)

;; Contexts for multiple accounts
(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Personal"
          :match-func (lambda (msg)
                       (when msg
                         (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "your.email@personal.com")
                 (user-full-name . "Your Name")
                 (mu4e-sent-folder . "/Personal/Sent")
                 (mu4e-drafts-folder . "/Personal/Drafts")
                 (mu4e-trash-folder . "/Personal/Trash")
                 (mu4e-refile-folder . "/Personal/Archive")))
        
        ,(make-mu4e-context
          :name "Work"
          :match-func (lambda (msg)
                       (when msg
                         (string-prefix-p "/Work" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "your.email@work.com")
                 (user-full-name . "Your Name")
                 (mu4e-sent-folder . "/Work/Sent")
                 (mu4e-drafts-folder . "/Work/Drafts")
                 (mu4e-trash-folder . "/Work/Trash")
                 (mu4e-refile-folder . "/Work/Archive")))))

;; Bookmarks
(setq mu4e-bookmarks
      '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
        (:name "Today's messages" :query "date:today..now" :key ?t)
        (:name "Last 7 days" :query "date:7d..now" :key ?w)
        (:name "Important" :query "flag:flagged" :key ?i)))

;; HTML email settings
(setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
      org-msg-startup "hidestars indent inlineimages"
      org-msg-default-alternatives '((new . (text html))
                                   (reply-to-html . (text html))
                                   (reply-to-text . (text))))

;; SMTP configuration
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Attachments
(setq mu4e-attachment-dir "~/Downloads"
      mu4e-view-use-gnus t
      mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;; Display settings
(setq mu4e-headers-fields
      '((:date . 25)
        (:flags . 6)
        (:from . 22)
        (:subject . nil))
      mu4e-headers-date-format "%Y-%m-%d %H:%M"
      mu4e-headers-time-format "%H:%M"
      mu4e-headers-include-related t)

;; Actions
(setq mu4e-marks
      '((refile
         :char ("r" . "▶")
         :prompt "refile"
         :dyn-target (lambda (target msg) (mu4e-get-refile-folder msg))
         :action (lambda (docid msg target) (mu4e--server-move docid target)))
        (delete
         :char ("D" . "␡")
         :prompt "Delete"
         :show-target (lambda (target) "delete")
         :action (lambda (docid msg target) (mu4e--server-remove docid)))))

;; Org integration
(setq org-mu4e-convert-to-html t
      org-mu4e-link-query-in-headers-mode nil)

(provide 'config-mail)
;;; config-mail.el ends here
