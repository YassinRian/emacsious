;;; Ugrep integration with Emacs

(require 'pretty-hydra)

(require 'pretty-hydra)

(defvar my-ugrep-options '("-r")
  "List of current ugrep options.")

(defun my-ugrep-toggle-option (option)
  "Toggle OPTION in `my-ugrep-options'."
  (if (member option my-ugrep-options)
      (setq my-ugrep-options (delete option my-ugrep-options))
    (add-to-list 'my-ugrep-options option))
  (message "ugrep options: %s" (string-join my-ugrep-options " ")))

(defun my-ugrep-option-p (option)
  "Return t if OPTION is enabled, nil otherwise."
  (if (member option my-ugrep-options) t nil))

(defun my-ugrep-get-filter (prefix)
  "Get filter option starting with PREFIX."
  (or (cadr (member (concat "--" prefix) my-ugrep-options))
      (let ((found nil))
        (dolist (opt my-ugrep-options)
          (when (string-prefix-p (concat "--" prefix) opt)
            (setq found (substring opt (+ 2 (length prefix))))))
        found)
      ""))

(pretty-hydra-define hydra-ugrep (:title "Ugrep Search Options" :quit-key "q" :color pink)
  ("Options"
   (("r" (my-ugrep-toggle-option "-r") (format "Recursive [%s]" (if (my-ugrep-option-p "-r") "x" " ")))
    ("i" (my-ugrep-toggle-option "-i") (format "Ignore case [%s]" (if (my-ugrep-option-p "-i") "x" " ")))
    ("w" (my-ugrep-toggle-option "-w") (format "Whole word [%s]" (if (my-ugrep-option-p "-w") "x" " ")))
    ("l" (my-ugrep-toggle-option "-l") (format "List files only [%s]" (if (my-ugrep-option-p "-l") "x" " ")))
    ("c" (my-ugrep-toggle-option "-c") (format "Count matches [%s]" (if (my-ugrep-option-p "-c") "x" " "))))
   
   "Filters"
   (("e" (let ((ext (read-string "File extension (e.g. *.el): ")))
           (setq my-ugrep-options 
                 (cl-remove-if (lambda (opt) (string-prefix-p "--include" opt)) my-ugrep-options))
           (when (not (string-empty-p ext))
             (add-to-list 'my-ugrep-options (format "--include=%s" ext))))
         (format "File ext: %s" (my-ugrep-get-filter "include")))
    
    ("x" (let ((pattern (read-string "Exclude pattern: ")))
           (setq my-ugrep-options 
                 (cl-remove-if (lambda (opt) (string-prefix-p "--exclude" opt)) my-ugrep-options))
           (when (not (string-empty-p pattern))
             (add-to-list 'my-ugrep-options (format "--exclude=%s" pattern))))
         (format "Exclude: %s" (my-ugrep-get-filter "exclude")))
    
    ("d" (let ((depth (read-string "Max depth: ")))
           (setq my-ugrep-options 
                 (cl-remove-if (lambda (opt) (string-prefix-p "--depth" opt)) my-ugrep-options))
           (when (not (string-empty-p depth))
             (add-to-list 'my-ugrep-options (format "--depth=%s" depth))))
         (format "Depth: %s" (my-ugrep-get-filter "depth")))
    
    ("t" (let ((type (read-string "File type (e.g. elisp): ")))
           (setq my-ugrep-options 
                 (cl-remove-if (lambda (opt) (string-prefix-p "--type" opt)) my-ugrep-options))
           (when (not (string-empty-p type))
             (add-to-list 'my-ugrep-options (format "--type=%s" type))))
         (format "Type: %s" (my-ugrep-get-filter "type"))))
   
   "Actions"
   (("s" (progn
           (call-interactively 'consult-dir)
           (let* ((pattern (read-string "ugrep pattern: " nil 'grep-history))
                  (command (format "ug --color=always -n %s %s ." 
                                  (string-join my-ugrep-options " ")
                                  (shell-quote-argument pattern))))
             (grep command)))
         "Search")
    ("p" (setq my-ugrep-options '("-r")) "Reset options")
    ("q" nil "Quit"))))

(defun my-ugrep-with-options ()
  "Run ugrep with options selected via pretty-hydra."
  (interactive)
  (hydra-ugrep/body))


(provide 'config-ugrep)
