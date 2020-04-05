(defvar my-saved-captures-dir "~/log/")
(defvar saved-capture-file-number nil "capture file rotation number")
(add-to-list 'savehist-additional-variables 'saved-capture-file-number)

(defun my/buffer-to-file-or-new-buffer (&optional to-file)
  (interactive "P")
  (if to-file
      (my/buffer-or-current-kill-to-new-buffer)
    (my/buffer-or-current-kill-to-file)))

(defun my/current-kill-to-file-or-new-buffer (&optional to-file)
  (interactive "P")
  (if to-file
      (my/buffer-or-current-kill-to-file t)
    (my/buffer-or-current-kill-to-new-buffer t)))

(defun my/buffer-or-current-kill-to-file (&optional kill)
  (let* ((file (concat my-saved-captures-dir
                       "capture"
                       (number-to-string saved-capture-file-number)))
         (filename (read-string "filename: " file nil file)))
    (if (file-exists-p filename)
        (delete-file filename))
    (if kill
        (write-region (current-kill 0) nil filename)
      (write-region nil nil filename))
    (find-file filename)
    (if (and (string= file filename) (= saved-capture-file-number 63))
        (setq saved-capture-file-number 0)
      (setq saved-capture-file-number (+ 1 saved-capture-file-number)))))

(defvar new-buffer-number 0)

(defun my/buffer-or-current-kill-to-new-buffer (&optional kill)
  (let* ((buffer-name  (concat "buffer" (number-to-string new-buffer-number)))
         (buffer (get-buffer buffer-name))
         (content (if kill (current-kill) (buffer-substring-no-properties (point-min) (point-max)))))
    (if (bufferp buffer)
        (kill-buffer buffer))
    (pop-to-buffer-same-window (get-buffer-create buffer-name))
    (insert content)))

(add-to-list 'auto-mode-alist '("buffer[0-9]+\\'" . compilation-mode))

(provide 'init-capture)
