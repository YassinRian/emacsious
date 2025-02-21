(defun get-current-theme-bg ()
  "Get the current theme's background color."
  (face-background 'default))

(defun update-alacritty-colors ()
  "Update Alacritty colors based on current Emacs theme."
  (interactive)
  (let* ((bg-color (get-current-theme-bg))
         (config-file "~/.config/alacritty/alacritty.toml")
         (temp-file "/tmp/alacritty-temp.toml")
         (color-format (substring bg-color 1)))  ; Remove # from color
    
    (with-temp-file temp-file
      (insert-file-contents config-file)
      (goto-char (point-min))
      (while (re-search-forward "background = \"0x[0-9A-Fa-f]+\"" nil t)
        (replace-match (format "background = \"0x%s\"" color-format))))
    
    (rename-file temp-file config-file t)))

;; Add this to ef-themes hooks
(add-hook 'ef-themes-post-load-hook #'update-alacritty-colors)
