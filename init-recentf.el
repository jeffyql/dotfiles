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

(defun my/recentf-by-type (filter &optional include)
  (let* ((list (if include
                    (cl-remove-if-not  (lambda (x) (string-match filter x)) recentf-list)
                  (cl-remove-if  (lambda (x) (string-match filter x)) list)))
         (list (if (equal (buffer-file-name) (car list))
                   (cdr list) list)))
    (ivy-read "Select file: " list
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :caller 'counsel-recentf)))

(defun my/get-recentf-project (filter &optional arg)
  (let (project-root root)
    (unless arg
      (setq project-root 
            (catch 'done
              (cl-loop for f in recentf-list do
                       (when (string-match-p filter f)
                         (setq root (projectile-project-root (file-name-directory f)))
                         (if root (throw 'done root)))))))
    (unless project-root
      (setq project-root 
            (projectile-project-root (projectile-completing-read
                                      "Switch to project: " projectile-known-projects))))
    project-root))

(defun my/get-not-displayed-from-file-list (list)
  (let* ((next-buf (unless (one-window-p) (window-buffer (next-window))))
         (next-file (if next-buf (buffer-file-name next-buf))))
    (if buffer-file-name (setq list (delete buffer-file-name list)))
    (if next-file (setq list (delete next-file list)))
    list))
  
(defun my/recentf-main-language-by-project (&optional arg)
  (interactive "P")
  (let* ((project-root (my/get-recentf-project my-main-language-filter arg))
         (list (cl-remove-if-not (lambda (f) (and (string-match my-main-language-filter f)
                                                  (string-prefix-p project-root f)))
                                 recentf-list))
         (list (my/get-not-displayed-from-file-list list)) 
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
   (my/recentf-by-type "\\.el$\\|\\.el.gz$" t))
 
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

 (defun my/recentf-misc (&optional arg)
   (interactive "P")
   (my/recentf-by-type (concat my-main-language-filter "\\|\\.org$\\|\\.el$\\|\\.el\\.gz") nil arg))

 (defun my/last-file-by-type (filter &optional negate)
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
      (cl-loop for file in list do
               (if negate
                   (unless (string-match-p filter file)
                     (throw 'done file))
                 (if (string-match-p filter file)
                        (throw 'done file)))))))

(defun my/open-last-file-by-type (filter)
  (let ((file (my/last-file-by-type filter)))
    (when (and file (file-exists-p file))
      (find-file file))))

(defun my/last-file-by-type-and-project (filter project)
  (let (file buf)
    (catch 'done
      (cl-loop for file in recentf-list do
               (setq buf (get-file-buffer file))
               (if (and (string-match-p filter file)
                        (not (and buf (get-buffer-window buf)))
                        (string-prefix-p project file))
                   (throw 'done file))))))

(defun my/open-last-file-by-type-and-project (filter project)
  (let ((file (my/last-file-by-type-and-project filter project)))
    (when (and file (file-exists-p file))
      (find-file file))))

(defun my/open-last-main-language-file (&optional arg)
  (interactive "P")
  (let ((project (my/get-recentf-project my-main-language-filter arg)))
    (my/open-last-file-by-type-and-project
     my-main-language-filter
     project)))

(defun my/last-log-file ()
  (interactive)
  (let (file)
    (setq file (my/last-file-by-type "capture[0-9]+\\|\\.log$"))
    (if (file-exists-p file)
        (find-file file) 
      (message "no log file found"))))


(defun my/open-last-file-by-type-and-project (filter project)
  (let ((file (my/last-file-by-type-and-project filter project)))
    (when (and file (file-exists-p file))
      (find-file file))))

(defun my/open-last-org-file ()
  (interactive)
  (let ((file (my/last-file-by-type "\\.org$")))
    (if (file-exists-p file)
        (find-file file))))

 (defun my/projectile-find-file ()
   (interactive "P")
   (let ((default-directory (projectile-completing-read
                             "Switch to project: " projectile-known-projects)
                               ))
     (projectile-find-file)))

 (defun my/projectile-dired ()
   (interactive)
   (let ((default-directory (projectile-completing-read
                             "Switch to project: " projectile-known-projects)))
     (projectile-dired)))

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
