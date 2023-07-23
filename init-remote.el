(setq selected-server-group nil)

(defvar linux-server-group
  '(
    "10.0.0.1"
    ))

(setq remote-server-group-list
      (list
       'linux-server-group
       ))

(defun my/init-vterm-remote ()
  (let* ((buffer-name (buffer-name))
         (buffer-name-root (car (split-string (buffer-name)))))
    (when (member buffer-name-root (symbol-value selected-server-group))
      (vterm-send-string (concat "ssh jeff@" buffer-name-root))
      (vterm-send-return))))

(add-hook 'vterm-mode-hook 'my/init-vterm-remote)

(defun my/select-remote-server ()
  (let (server-code server-num)
    (if selected-server-group
        (setq server-code (my/quick-selection-select-item selected-server-group)))
    (if (or (not selected-server-group) (equal server-code ?\s))
        (setq selected-server-group (my/quick-selection-select-group remote-server-group-list)
              server-code (my/quick-selection-select-item selected-server-group)))
    (if (equal server-code ?\s)
        (error "no server is selected"))
    (setq server-num (string-to-number (char-to-string server-code)))
    (nth (1- server-num) (symbol-value selected-server-group))
    ))

(defun my/goto-remote-server-root-dir ()
  (interactive)
  (let* ((remote-server (my/select-remote-server))
         (dir (concat "/ssh:" remote-server ":/")))
    (message "")
    (dired dir)
    (message "")))

(defun my/select-remote-server-vterm ()
  (interactive)
  (let* ((base (event-basic-type last-input-event))
         (vterm (cl-digit-char-p base))
         (remote-server (my/select-remote-server)))
    (my/get-create-vterm (concat remote-server (concat " (" (number-to-string vterm) ")")))
    (message "")
    ))


(provide 'init-remote)
