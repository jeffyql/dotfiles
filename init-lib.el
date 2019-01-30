;; buffer

(defun my/switch-indirect-narrow (&optional arg)
  (interactive "P")
  (if (buffer-base-buffer)
      (let ((start (window-start))
            (pos (point))
            (buf (current-buffer)))
        (switch-to-buffer (buffer-base-buffer))
        (set-window-start (selected-window) start)
        (goto-char pos) ;; anchor cursor
        (unless (= (window-start) (point-min))
          (scroll-down 1))
        (goto-char pos)
        (if (equal arg '(4))
            (kill-buffer buf)))
    (let* ((pos (point))
           (win-start (window-start))
           (win-end (window-end))
           (region (if mark-active t))
           (file-name (buffer-name (buffer-base-buffer)))
           (prefix
            (when (and (not (equal arg '(4))) (not region))
              (cond 
               ((derived-mode-p 'prog-mode)
                (which-function))
               ((eq major-mode 'org-mode)
                (org-get-heading t t))
               (t nil))))
           buf-name beg end)
      (unless prefix
        (setq prefix (read-string (concat "buffer name prefix: "))))
      (setq buf-name (concat ">><<" prefix "::" file-name))
      (if (get-buffer buf-name)
          (progn 
            (switch-to-buffer buf-name)
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
        (remove-overlays beg end)
        (clone-indirect-buffer buf-name nil)
        (switch-to-buffer buf-name nil t)
        (cond
         (region
          (narrow-to-region beg end))
         ((derived-mode-p 'prog-mode)
          (narrow-to-defun))
         ((eq major-mode 'org-mode)
          (org-narrow-to-subtree)))
        (goto-char pos)
        (recenter nil)))))

;; file

(defvar my/counsel-recentf-file-extension nil)

(defun my/counsel-recentf-set-file-extension ()
  (interactive)
  (let ((extension (concat "\\." (read-string "file extension string:") "$")))
    (customize-save-variable 'my/counsel-recentf-file-extension extension)
  ))

(defun my/counsel-recentf-misc ()
  (interactive)
  (let ((filter "\\.c$\\|\\.h$\\|\\.cpp$\\|\\.hpp$\\|\\.el$\\|\\.el.gz$\\|\\.json$\\|\\.log$\\|\\.org$\\|\\.py$\\|\\.sh$\\|\\.xml$\\|Dockerfile\\|\\.[cs]ql$")
        (file-list (mapcar #'substring-no-properties recentf-list)))
    (ivy-read "Select file: " (cl-remove-if  (lambda (x) (string-match filter x)) file-list)
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :caller 'counsel-recentf)))

(defun my/counsel-recentf-filtered (filter &optional project-root)
  (let (file-list)
    (setq file-list
          (if project-root
              (cl-remove-if-not (lambda (f)
                                  (and (string-prefix-p project-root f)
                                       (string-match filter f)))
                                recentf-list)
            (cl-remove-if-not  (lambda (x) (string-match filter x)) recentf-list))
          file-list (if buffer-file-name
                        (delete buffer-file-name file-list)
                      file-list))
    (if project-root
        (setq file-list (mapcar (lambda (f) (file-relative-name f project-root)) file-list)))
    (ivy-read (concat "[" project-root "] Select file: ") file-list
              :action (lambda (f)
                        (with-ivy-window
                          (find-file (if project-root (expand-file-name f project-root)
                                       f))))
              :caller 'counsel-recentf)))

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
