
(defun my/send-command ()
  (interactive)
  (let ((buf (unless (one-window-p) (window-buffer (next-window))))
        mode beg end cmd-str window)
    (if (one-window-p)
        (error "only one window"))
    (with-current-buffer buf
      (unless (or (eq major-mode 'vterm-mode)
                  (eq major-mode 'sql-interactive-mode))
        (error "next buffer not a shell buffer"))
      (setq mode major-mode))
    (cond
     ((use-region-p)
      (setq beg (region-beginning) 
            end (region-end))
      (deactivate-mark t))
     ((and (eq major-mode 'org-mode) (org-in-item-p))
      (save-excursion
        (beginning-of-line)
        (looking-at org-list-full-item-re)
        (setq pos (car (last (match-data))))
        (goto-char pos)
        (if (looking-at "[ \t]*\\(\\$\\|#\\|>\\)[ \t]+")
            (goto-char (match-end 0)))
        (setq beg (point)
              end (line-end-position))))
     ((and (eq major-mode 'org-mode) (org-in-src-block-p))
      (setq cmd-str (org-remove-indentation (org-element-property :value (org-element-context)))))
     (t
      (save-excursion
        (setq beg (line-beginning-position)
              end (line-end-position)))))
    (unless cmd-str
      (setq cmd-str (buffer-substring-no-properties beg end)))
    (cond
     ((eq mode 'sql-interactive-mode)
      (let ((sql-buffer buf))
        (sql-send-string cmd-str)))
     ((eq mode 'vterm-mode)
      (with-current-buffer buf
        (vterm-send-string cmd-str)
        (vterm-send-return)))
     (t)
     )))

(provide 'init-send-command)
