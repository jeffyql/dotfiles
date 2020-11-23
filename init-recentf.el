;; recent files 1) by type (el org), 2) by project (using whatever criterion), or 3) a single group of all other files

(defvar my-project-hint-files-filter "\\.h$\\|\\.c$\\|\\.cpp$\\|\\.hpp$")

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
    (ivy-read "Select file: " list
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :caller 'counsel-recentf)))

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
         (list (cl-remove-if-not (lambda (f) (string-match-p my-project-hint-files-filter f)) recentf-list))
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

(defun my/recentf-org ()
  (interactive)
  (let* ((list (cl-remove-if-not  (lambda (x) (string-match "\\.org$" x)) recentf-list))
         (list (if (equal (buffer-file-name) (car list)) (cdr list) list))
         (alist (mapcar (lambda (f) (cons (org-roam--get-title-or-slug f) f)) list))
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


(defun my/last-file-by-type (filter)
  (let ((next-buf (unless (one-window-p) (window-buffer (next-window))))
        (list recentf-list))
    (if next-buf
        (with-current-buffer next-buf
          (when buffer-file-name
            (recentf-add-file buffer-file-name)
            (setq list (cdr list)))))
    (when buffer-file-name
      (setq list (cdr list)))
    (catch 'done
      (cl-loop for f in list do
               (if (stringp filter)
                   (if (string-match-p filter f)
                       (throw 'done f))
                 (if (funcall filter f)
                     (throw 'done f)))))))

(defun my/open-last-file-by-type (filter)
  (let ((file (my/last-file-by-type filter)))
    (when (and file (file-exists-p file))
      (find-file file))))

(defun my/open-last-file-by-project ()
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

(defun my/open-last-org-file (&optional arg)
  (interactive "P")
  (if arg
    (dired my-org-dir)
    (let ((file (my/last-file-by-type "\\.org$")))
      (find-file file))))

(defun my/last-log-file ()
  (interactive)
  (let (file)
    (setq file (my/last-file-by-type "capture[0-9]+\\|\\.log$"))
    (if (file-exists-p file)
        (find-file file)
      (message "no log file found"))))

(defun my/open-last-misc-file ()
  (interactive)
  (let ((file (my/last-file-by-type (lambda (f) (not (string-match-p (concat my-project-hint-files-filter "\\|\\.org$\\|\\.el$\\|\\.el\\.gz$") f))))))
    (if (file-exists-p file)
        (find-file file))))

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
