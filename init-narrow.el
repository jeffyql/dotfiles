(defun my/counsel-narrowed-indirect (&optional arg)
   (interactive "P")
   (let* ((buffer-names (mapcar #'buffer-name (buffer-list)))
          (narrowed-indirect-list
           (cl-remove-if-not
            (lambda (b) (and (> (length b) 4) (equal ">>"(substring b 0 2))))
            buffer-names)))
     (if arg
       (unless (symbol-at-point)
         (error "symbol not found at point")))

     (ivy-read "Select indirect buffer: " narrowed-indirect-list
               :action (lambda (b)
                         (with-ivy-window
                           (switch-to-buffer b)))
               :initial-input (if arg (thing-at-point 'symbol)))))


(defun my/switch-indirect-narrow (&optional arg)
  (interactive "P")
  (if (buffer-base-buffer)
      (let ((start (window-start))
            (pos (point))
            (buf (current-buffer)))
        (pop-to-buffer-same-window (buffer-base-buffer))
        (set-window-start (selected-window) start)
        (goto-char pos) ;; anchor cursor
        (unless (= (window-start) (point-min))
          (scroll-down 1))
        (goto-char pos))
    (let* ((pos (point))
           (win-start (window-start))
           (win-end (window-end))
           (region (if (use-region-p) t))
           (file-name (buffer-name (buffer-base-buffer)))
           (prefix
            (when (and (not (equal arg '(4))) (not region))
              (cond
               ((eq major-mode 'c++-mode)
                (car (c-defun-name-and-limits nil)))
               ((derived-mode-p 'prog-mode)
                (which-function))
               ((eq major-mode 'org-mode)
                (org-get-heading t t))
               (t nil))))
           buf-name beg end)
      (unless prefix
        (setq prefix (read-string (concat "buffer name prefix: "))))
      (setq buf-name (concat ">>" prefix "<< [file:" file-name "]"))
      (if (get-buffer buf-name)
          (progn
            (pop-to-buffer-same-window buf-name)
            (if (and (>= win-start (point-min))
                     (<= win-end (point-max)))
                (set-window-start (selected-window) win-start))
            (goto-char pos)
            )
        (when region
          (setq  beg (region-beginning)
                 end (region-end))
          (deactivate-mark t)
          )
        ;; (remove-overlays beg end)
        (clone-indirect-buffer buf-name nil)
        (pop-to-buffer-same-window buf-name)
        (cond
         (region
          (narrow-to-region beg end))
         ((derived-mode-p 'prog-mode)
          (narrow-to-defun))
         ((eq major-mode 'org-mode)
          (org-narrow-to-subtree)))
        (goto-char pos)
        (recenter nil)))))

 (defun my/goto-indirect-narrow-at-point ()
   (interactive)
   (let ((buffer-names (mapcar (lambda (b) (buffer-name b)) (buffer-list)))
         (func-name (thing-at-point 'symbol))
         buf-name)
     (unless func-name
       (error "no symbol found at point"))
     (setq func-name (concat func-name "<<"))
     (setq buf-name (seq-find (lambda (s) (and (> (length s) 4)
                                               (equal ">>" (substring s 0 2))
                                               (string-match-p (regexp-quote func-name) s)))
                              buffer-names))
     (unless (stringp buf-name)
       (error "buffer not found"))
     (switch-to-buffer buf-name)))

(provide 'init-narrow)
