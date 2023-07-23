(defun my/init-vterm-remote ()
  (let* ((buffer-name (buffer-name))
         (buffer-name-root (car (split-string buffer-name))))
    (when (member buffer-name-root (symbol-value selected-server-group))
      (vterm-send-string (concat "ssh " (my/get-hostname buffer-name-root)))
      (vterm-send-return))))

(add-hook 'vterm-mode-hook 'my/init-vterm-remote)

(defun my/get-test-server-vterm-name (server &optional nth)
  (let* ((buf-name (concat (nth (1- (string-to-number server)) (symbol-value selected-server-group)) " [" server "]"))
         (buf-name (if nth (concat buf-name " (" nth ")") buf-name)))
    buf-name))

(defun my/goto-test-server-root-dir ()
  (interactive)
  (let* (server-code server-num hostname dir)
    (if selected-server-group
        (setq server-code (my/quick-selection-select-item selected-server-group)))
    (if (or (not selected-server-group) (equal server-code ?\s))
        (setq selected-server-group (my/quick-selection-select-group server-group-list)
              server-code (my/quick-selection-select-item selected-server-group)))
    (if (equal server-code ?\s)
        (message "no server is selected")
      (setq server-num (string-to-number (char-to-string server-code))
            hostname (my/get-hostname (nth (1- server-num) (symbol-value selected-server-group)))
            dir (concat "/ssh:" hostname ":/"))
      (dired dir)
      (message ""))))

(defun my/select-test-server-vterm ()
  (interactive)
  (let* ((base (event-basic-type last-input-event))
         (vterm (cl-digit-char-p base))
         server-code server-num)
    (if selected-server-group
        (setq server-code (my/quick-selection-select-item selected-server-group)))
    (if (or (not selected-server-group) (equal server-code ?\s))
        (setq selected-server-group (my/quick-selection-select-group server-group-list)
              server-code (my/quick-selection-select-item selected-server-group)))
    (if (equal server-code ?\s)
        (message "no server is selected")
      (setq server-num (string-to-number (char-to-string server-code)))
      (my/get-create-vterm (concat (nth (1- server-num) (symbol-value selected-server-group))
                                   (concat " (" (number-to-string vterm) ")")))
      (message "")
    )))



(provide 'init-server)
