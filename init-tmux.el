(defun my/tmux-toggle-zoom ()
  (interactive)
  (tmux-toggle-zoom)
  (unless (one-window-p)
    (delete-other-windows)))

(defun tmux-ipython-send-buffer (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (tmux-ipython-reset))
  (tmux-run-key (buffer-substring-no-properties (point-min) (point-max)))
  )

(defun tmux-python-console-p ()
  (let ((shell (tmux-pane-1-login-type)))
    (and (stringp shell)
         (>= (length shell) 6)
         (equal "python" (substring shell 0 6)))))

(defun tmux-ipython-send-region (&optional arg)
  (interactive "P")
  (let* (beg end cmd-str no-history)
    (unless (tmux-python-console-p)
        (error "must run in a ipthon shell"))
    (if (use-region-p)
        (progn
          (setq beg (region-beginning) 
                end (region-end))
          (deactivate-mark t)
          (unless (= (line-number-at-pos beg) (line-number-at-pos end))
            (setq no-history t)))
      (save-excursion
        (setq beg (line-beginning-position)
              end (line-end-position))))
    (setq cmd-str (buffer-substring-no-properties beg end))
    (tmux-shell-send-string cmd-str)
    (unless comint-input-ring
      (comint-read-input-ring 'silent))
    (unless no-history
      (comint-add-to-input-history cmd-str))
    (forward-line)
    (python-util-forward-comment 1)))

(defun tmux-ipython-send-defun ()
  (interactive)
  (let (beg end)
    (save-excursion
      (setq beg
            (progn
              (end-of-line 1)
              (while (and (or (python-nav-beginning-of-defun)
                              (beginning-of-line 1))
                          (> (current-indentation) 0)))
              (point-marker))
            end
            (progn
              (or (python-nav-end-of-defun)
                  (end-of-line 1))
              (point-marker))))
    (tmux-shell-send-string
     (buffer-substring-no-properties beg end))
    (goto-char end)
    (python-util-forward-comment 1)))

(defun tmux-ipython-send-from-beginning ()
  (interactive)
  (tmux-shell-send-string
   (buffer-substring-no-properties
    (point-min)
    (line-end-position)
    )))

(defun tmux-ipython-send-to-end ()
  (interactive)
  (let (pos)
    (save-excursion
      (save-restriction
        (if (re-search-forward "^pass *$" nil t)
            (setq pos (match-beginning 0)))))
    (tmux-shell-send-string
     (buffer-substring-no-properties
      (line-beginning-position)
      (or pos (point-max))))))

(defun tmux-ipython-print (&optional arg)
  (interactive "P")
  (tmux-ipython-send-special 'print))

(defun tmux-ipython-help (&optional arg)
  (interactive "P")
  (tmux-ipython-send-special 'help))

(defun tmux-ipython-send-special (type &optional arg)
  (let (object)
    (setq object
          (cond
           ((equal arg '(4))
            (read-string "Input: " ))
           ((use-region-p)
            (buffer-substring-no-properties (region-beginning) (region-end)))
           ((symbol-at-point)
            (thing-at-point 'symbol))
           (t (error "No symbol at point"))))
    (setq string
          (if (eq type 'help)
              (concat "help(" object ")")
            object))
    (tmux-run-key string)))

(defun tmux-python-run-this-file ()
  (interactive)
  (unless (tmux-ipython-console-p)
    (error "must run inside a pthon interpreter"))
  (if (buffer-modified-p)
      (save-buffer))
  (tmux-run-key (concat "run " (buffer-file-name))))

(defun tmux-ipython-start ()
  (interactive)
  (tmux-run-key "ipython"))

(defun tmux-ipython-start-existing ()
  (interactive)
  (let (buf)
    (setq buf (get-buffer "*Python*"))
    (if (bufferp buf)
        (if (y-or-n-p "kill current ipython console?")
            (kill-buffer buf)
          (error "already in ipython console")))
    (tmux-run-key "jupyter console --existing ")))


(provide 'init-tmux)
