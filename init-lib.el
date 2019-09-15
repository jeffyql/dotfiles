
(defvar my/counsel-recentf-file-extension nil)

(defun my/counsel-recentf-set-file-extension ()
  (interactive)
  (let ((extension (concat "\\." (read-string "file extension string:") "$")))
    (customize-save-variable 'my/counsel-recentf-file-extension extension)
  ))


(setq my/counsel-rg-base-command-prefix
      "rg -i -M 512 --no-heading --line-number --color never --follow ")

(defun my/counsel-rg-at-point (&optional arg)
  (interactive "P")
    (setq current-prefix-arg nil)
    (counsel-rg (and (equal arg '(4)) (symbol-at-point) (thing-at-point 'symbol))))

(defun my/ripgrep-run (arguments)
  (compilation-start
   (mapconcat 'identity (append '("rg -i -M 512 --no-heading --vimgrep --follow -n ")
                                arguments) " ") 'ripgrep-search-mode))

(defun my/ripgrep-regexp (&optional arg)
  (interactive "P")
  (let* ((regexp 
          (if (not (equal arg '(4)))
              (read-string "Ripgrep search for: ")
            (unless (symbol-at-point)
              (error "symbol not found at point"))
            (thing-at-point 'symbol)))
         (arguments (list (shell-quote-argument regexp) ".")))
    (my/ripgrep-run arguments)
    ))

(defun my/ripgrep-this-file (&optional arg)
  (interactive "P")
  (let* ((buf-name (if (equal arg '(4))
                       (read-string "Search result buffer name: ")))
         (regexp (read-string "Ripgrep search for: "))
         (fn (file-name-nondirectory
              (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (buffer-file-name (or (buffer-base-buffer) (current-buffer))))))
         (arguments (list "-g" (concat "'" fn "'") (shell-quote-argument regexp) ".")))
    (message "%s" arguments)
    (my/ripgrep-run arguments)
    (if (equal arg '(4))
        (with-current-buffer "*ripgrep-search*"
          (rename-buffer (concat "*" buf-name "-ripgrep-search*")))))
  )

(defun my/ripgrep-regexp-options (&optional arg)
  (interactive "P")
  (let* ((regexp 
          (if (not (equal arg '(4)))
              (read-string "Ripgrep search for: ")
            (unless (symbol-at-point)
              (error "symbol not found at point"))
            (thing-at-point 'symbol)))
         (options
          (read-string "additional command-line options: "))
         (arguments (list (shell-quote-argument regexp) options ".")))
    (message "%s" arguments)
    (my/ripgrep-run arguments)
    ))

(defun get-buffer-file-name (&optional arg)
  " "
  (interactive "P")
  (let* ((buffer (or (buffer-base-buffer) (current-buffer)))
         (file (buffer-file-name buffer))
         (capture
          (if (equal arg '(4))
              (file-name-nondirectory file)
            file)))
    (kill-new capture)
    (x-select-text capture)
    (message "%s" capture)))

(defun my/ffap ()
  (let* ((file (ffap-string-at-point))
         (dir (ivy-dired-history--read-file-name "directory: "))
         (file-name (concat dir file)))
    (when (file-exists-p file-name)
      (ivy-dired-history--update dir)
      (find-file file-name))))

(provide 'init-lib)
