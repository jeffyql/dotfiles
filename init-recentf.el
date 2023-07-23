;; recent files 1) by type (el org), 2) by project (using whatever criterion), or 3) a single group of all other files
(require 'consult)

(defvar my-project-hint-files-filter "\\.py$")

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :custom
  (projectile-current-project-on-switch 'keep)
  (projectile-completion-system 'ivy)
  (projectile-generic-command "fd . -0")
  (projectile-git-command "fd . -0")
  )

(defun my/recentf-by-type (filter &optional exclude)
  (let* ((next-buf (unless (one-window-p) (window-buffer (next-window))))
         list)
    (setq list (if exclude
                   (cl-remove-if  (lambda (x) (string-match filter x)) recentf-list)
                 (cl-remove-if-not  (lambda (x) (string-match filter x)) recentf-list)))
    (if next-buf
        (setq list (delete (buffer-file-name next-buf) list)))
    (setq list (delete buffer-file-name list))
    (find-file
     (consult--read (mapcar #'abbreviate-file-name list)
                    :prompt "Find recent file: "
                    :sort nil
                    :require-match t
                    :category 'file
                    ;; :state (consult--file-preview)
                    ))))

(defun my/get-project-by-recentf (&optional switch-project)
  (let (project-root first root)
    (unless switch-project
      (setq first (cl-find-if (lambda (f) (string-match-p my-project-hint-files-filter f)) recentf-list)
            project-root (if first (projectile-project-root (file-name-directory first)))))
    (unless project-root
      (setq project-root
            (projectile-project-root (projectile-completing-read
                                      "Switch to project: " projectile-known-projects))))
    project-root))

(defun my/remove-displayed-files (list)
  (let* ((next-buf (unless (one-window-p) (window-buffer (next-window))))
         (next-file (if next-buf (buffer-file-name next-buf))))
    (if buffer-file-name (setq list (delete buffer-file-name list)))
    (if next-file (setq list (delete next-file list)))
    list))
 
(defun my/recentf-by-project (&optional switch-project)
  (interactive "P")
  (let* ((project-root (my/get-project-by-recentf switch-project))
         (list (cl-remove-if-not (lambda (f) (and (string-prefix-p project-root f)
                                                  (string-match-p my-project-hint-files-filter f))) recentf-list))
         (list (my/remove-displayed-files list))
         project-root-name prompt)
    (setq list (mapcar (lambda (f) (file-relative-name f project-root)) list)
          project-root-name (file-name-nondirectory (directory-file-name project-root))
          prompt (concat "[" project-root-name "]: "))
    (ivy-read  prompt list
               :action (lambda (f)
                         (with-ivy-window
                           (find-file (expand-file-name f project-root))))
               :caller 'counsel-recentf)))

(defun my/recentf-el ()
  (interactive)
  (my/recentf-by-type "\\.el$\\|\\.el.gz$"))

(defun my/recentf-code-file ()
  (interactive)
  (my/recentf-by-type "\\.py$\\|\\.sql$"))

(defun my/recentf-data-file ()
  (interactive)
  (my/recentf-by-type "\\.txt$\\|\\.csv$\\|\\.json$\\|\\.xml"))

(defun my/recentf-org ()
  (interactive)
  (let* ((list (cl-remove-if-not  (lambda (x) (string-match "\\.org$" x)) recentf-list))
         (list (if (equal (buffer-file-name) (car list)) (cdr list) list))
         (alist (mapcar (lambda (f) (cons (org-roam-db--get-title f) f)) list))
         (list (mapcar 'car alist)))
    (ivy-read "Select file: " list
              :action (lambda (f)
                        (with-ivy-window
                          (find-file (cdr (assoc f alist)))))
              ;; (message (cdr (assoc f alist)))))
              :caller 'my/recentf-org)))

(defun my/recentf-misc ()
  (interactive)
  (my/recentf-by-type (concat my-project-hint-files-filter "\\|\\.org$\\|\\.el$\\|\\.el\\.gz$") t))

(defun my/recentf-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs
         (delete-dups
          (mapcar (lambda (file)
                    (if (file-directory-p file) file (file-name-directory file)))
                  recentf-list))))

    (let ((dir (ivy-read "Directory: "
                         recent-dirs
                         :re-builder #'ivy--regex
                         :sort nil
                         :initial-input nil)))
      (dired dir))))

(defun my/last-file-by-type (filter)
  (let ((next-buf (unless (one-window-p) (window-buffer (next-window))))
        (list recentf-list))
    (if next-buf
        (with-current-buffer next-buf
          (when buffer-file-name
            (recentf-add-file buffer-file-name)
            (setq list (cdr list)))))
    (when (equal buffer-file-name (car list))
      (setq list (cdr list)))
    (catch 'done
      (cl-loop for f in list do
               (if (stringp filter)
                   (if (string-match-p filter f)
                       (throw 'done f))
                 (if (funcall filter f)
                     (throw 'done f)))))))

(defun my/goto-last-file-by-project ()
  (interactive)
  (let ((project (my/get-project-by-recentf))
         file buf)
    (unless project
      (error "project is not found."))
    (setq file
          (catch 'done
            (cl-loop for f in recentf-list do
                     (setq buf (get-file-buffer f))
                     (if (and (string-match-p my-project-hint-files-filter f)
                              (not (and buf (get-buffer-window buf)))
                              (string-prefix-p project f))
                         (throw 'done f)))))
    (when (and file (file-exists-p file))
      (find-file file))))

(defun my/goto-last-org-file (&optional arg)
  (interactive "P")
  (if arg
    (dired my-org-dir)
    (let ((file (my/last-file-by-type "\\.org$")))
    (if (file-exists-p file)
        (find-file file)))))

(defun my/goto-last-code-file ()
  (interactive)
  (let ((file (my/last-file-by-type "\\.py$\\|\\.sql$")))
    (if (file-exists-p file)
        (find-file file))))

(defun my/goto-last-data-file ()
  (interactive)
  (let ((file (my/last-file-by-type "\\.txt$\\|\\.xml$\\|\\.json$\\|\\.csv$")))
    (if (file-exists-p file)
        (find-file file))))

(defun my/goto-last-misc-file ()
  (interactive)
  (let ((file (my/last-file-by-type (lambda (f) (not (string-match-p (concat my-project-hint-files-filter "\\|\\.org$\\|\\.el$\\|\\.el\\.gz$") f))))))
    (if (file-exists-p file)
        (find-file file))))

(defun my/recent-buffers-by-type (pred)
  (interactive)
  (let* ((selected (cl-remove-if-not (lambda (b) (funcall pred b)) (buffer-list)))
         (buffer-names (mapcar #'buffer-name selected)))
    (consult--read names-buffer
     :prompt "Select indirect buffer: "
     :sort nil
     :require-match t
     )))

(defun my/recent-narrowed-indirect ()
  (interactive)
  (my/recent-buffers-by-type (lambda (b) (and (> (length (buffer-name b)) 4) (equal ">>"(substring (buffer-name b) 0 2))))))

(defun my/recent-search ()
  (interactive)
  (my/recent-buffers-by-type (lambda (b) (with-current-buffer b
                                           (or (eq major-mode 'ivy-occur-grep-mode)
                                               (eq major-mode 'deadgrep-mode))))))

(defun my/recent-vterms ()
  (interactive)
  (my/recent-buffers-by-type (lambda (b) (with-current-buffer b
                                           (eq major-mode 'vterm-mode)))))

(defun my/goto-last-buffer-by-type (pred)
  (catch 'done
    (cl-loop for buf in (buffer-list (selected-frame)) do
             (when (and (funcall pred buf)
                        (not (get-buffer-window buf)))
               (throw 'done buf)))))

(defun my/goto-last-narrowed-buffer ()
  (interactive)
  (let (buf)
    (setq buf (my/goto-last-buffer-by-type (lambda (b) (buffer-base-buffer b))))
    (if (bufferp buf)
        (pop-to-buffer-same-window buf)
      (error "no more narrowed buffer"))))

(defun my/goto-last-vterm-buffer ()
  (interactive)
  (let (buf buffer-name)
    (setq buf (my/goto-last-buffer-by-type (lambda (b) (with-current-buffer b (eq major-mode 'vterm-mode)))))
    (if (bufferp buf)
        (pop-to-buffer-same-window buf)
      (setq buffer-name 
            (if (eq major-mode 'vterm-mode)
                (if (string= (substring (buffer-name) 0 7) "vterm-1") "vterm-2" "vterm-1")
              "vterm-1")
            buffer-name (read-string "vterm buffer name: " buffer-name))
        (my/get-create-vterm buffer-name))))

(defun my/goto-last-search-buffer ()
  (interactive)
  (let (buf)
    (setq buf (my/goto-last-buffer-by-type (lambda (b) (with-current-buffer b
                                                         (or (eq major-mode 'ivy-occur-grep-mode)
                                                             (eq major-mode 'deadgrep-mode))))))
    (if (bufferp buf)
        (pop-to-buffer-same-window buf)
      (error "no more deadgrep buffer"))))

(defun my/ffap ()
  (interactive)
  (let (msg loc filename dir line))
  (if (or (symbol-value 'compilation-minor-mode)
          (eq major-mode 'compilation-mode))
      (setq msg (get-text-property (point) 'compilation-message)
            loc (and msg (compilation--message->loc msg))
            filename (caar (compilation--loc->file-struct loc))
            filename (file-name-nondirectory filename)
            line (cadr loc))
    (setq filename (ffap-string-at-point))
    (save-excursion
      (beginning-of-line 2)
      (if (looking-at "\\([0-9]+\\):")
          (setq line (string-to-number (match-string 1))))))
  (setq dir (ivy-dired-history--read-file-name "directory: ")
        filename (concat dir filename))
  (if (not (file-exists-p filename))
      (message "%s doesn't exist" filename)
    (ivy-dired-history--update dir)
    (find-file filename)
    (if line
        (goto-line line))))

(provide 'init-recentf)
