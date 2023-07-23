(setq my-servers
      '(
        "ubuntu1"
        "ubuntu2"
        "docker1"
        "docker2"
        )
      )
(defun my/init-vterm ()
  (let* ((buffer-name (buffer-name))
         (buffer-name-root (car (split-string buffer-name))))
    (when (member buffer-name-root (symbol-value selected-server-group))
      (vterm-send-string "ssh jeff@192.168.99.1")
      (vterm-send-return))))

(add-hook 'vterm-mode-hook 'my/init-vterm)

(defun my/select-vterm-by-number (&optional arg)
  (interactive "P")
  (let ((base (event-basic-type last-input-event))
        server)
    (when arg
      (unless (integerp arg)
        (error "prefix arg is not a digit"))
      (unless (< arg 9)
        (error "prefix is too big")))
    (setq server (cl-digit-char-p base))
    (my/check-test-server-selection server)
    (my/get-create-vterm (my/get-test-server-vterm-name (number-to-string server) (if arg (number-to-string arg))))))
(buf-name (concat (nth (1- (string-to-number server)) (symbol-value selected-server-group)) " [" server "]"))

(defun my/select-vterm-by-number ()
  (interactive)
  (let ((base (event-basic-type last-input-event))
        server)
    (setq server (cl-digit-char-p base))
    (unless (integerp server)
      (error "server input is not a digit number"))
    (when (not my-servers)
       (error "server group not found"))
    (setq group (symbol-value my-servers))
    (unless (<= server (length group))
      (error "server is digit is out of the range"))
    (setq buffer-name (concat (nth (1- (string-to-number server)) (symbol-value selected-server-group)) " [" server "]"))
    (my/get-create-vterm (my/get-test-server-vterm-name (number-to-string server) (if arg (number-to-string arg))))))
