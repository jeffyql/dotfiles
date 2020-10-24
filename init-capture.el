(defvar my-saved-captures-dir "~/log/")

(unless (bound-and-true-p saved-capture-file-number)
  (setq saved-capture-file-number 0))

(add-to-list 'savehist-additional-variables 'saved-capture-file-number)

(defun my/capture-by-file (&optional arg)
  (interactive "P")
  (unless saved-capture-file-number
    (setq saved-capture-file-number 0))
  (let* ((file (concat my-saved-captures-dir
                       "capture"
                       (number-to-string saved-capture-file-number)))
         (filename (read-string "filename: " file nil file)))
    (if (file-exists-p filename)
        (delete-file filename))
    (if arg
      (write-region nil nil filename))
    (find-file filename)
    (if (string= file filename)
      (if (= saved-capture-file-number 63)
          (setq saved-capture-file-number 0)
        (setq saved-capture-file-number (+ 1 saved-capture-file-number))))))

(defvar new-buffer-number 0)

(defun my/capture-by-buffer (&optional arg)
  (interactive "P")
  (let* ((buffer-name  (concat "buffer" (number-to-string new-buffer-number)))
         (buffer (get-buffer buffer-name))
         (content (if arg (buffer-substring-no-properties (point-min) (point-max)))))
    (if (bufferp buffer)
        (kill-buffer buffer))
    (pop-to-buffer-same-window (get-buffer-create buffer-name))
    (if content
        (insert content))
    (setq new-buffer-number (+ 1 new-buffer-number))
    (org-mode)))

(provide 'init-capture)
