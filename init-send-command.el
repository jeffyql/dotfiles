(defun my/send-to-repl-window ()
  (interactive)
  (if (one-window-p)
      (error "next window not found"))
  (let ((buf (window-buffer (next-window)))
        mode beg end cmd-str window)
    (with-current-buffer buf
      (unless (or (eq major-mode 'vterm-mode)
                  (eq major-mode 'sql-interactive-mode))
        (error "next window buffer is not a repl buffer"))
      (setq mode major-mode))
    (cond
     ((use-region-p)
      (setq beg (region-beginning) 
            end (region-end))
      (deactivate-mark t))
     ((code-cells-mode)
      (setq code-cells-boundary-regexp 
            (cond
             ((eq major-mode 'python-mode)
              "\\s<+\\(?:\\s-*%\\(?1:%+\\)\\|\\(?1:\\*+\\)\\| In\\[[[:space:][:digit:]]*]:\\)")      
             ((eq major-mode 'sql-mode)
              "^-- %%.*")
             )
            cell (code-cells--bounds)
            beg (car cell)
            end (cadr cell)))
     ((eq 'org-mode major-mode)
      (cond
       ((org-in-item-p)
        (save-excursion
          (beginning-of-line)
          (looking-at org-list-full-item-re)
          (setq pos (car (last (match-data))))
          (goto-char pos)
          (if (looking-at "[ \t]*\\(\\$\\|#\\|>\\)[ \t]+")
              (goto-char (match-end 0)))
          (setq beg (point)
                end (line-end-position))))
       ((org-in-src-block-p)
        (setq cmd-str (org-remove-indentation (org-element-property :value (org-element-context)))))
       (t nil)))
     (t
      (save-excursion
        (beginning-of-line)
        (if (looking-at "[ \t]*\\(\\$\\|#\\|>\\)[ \t]+")
            (goto-char (match-end 0)))
        (setq beg (point)
              end (line-end-position)))))
    (unless cmd-str
      (setq cmd-str (concat (buffer-substring-no-properties beg end) "\n")))
    (cond
     ((eq mode 'sql-interactive-mode)
      (let ((sql-buffer buf))
        (sql-send-string cmd-str)))
     ((eq mode 'vterm-mode)
      (with-current-buffer buf
        (vterm-send-string cmd-str)
        ))
     (t)
     )))

(provide 'init-send-command)
