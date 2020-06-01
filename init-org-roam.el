
(use-package org-roam
      :hook 
      (after-init . org-roam-mode)
      :custom
      (org-roam-completion-system 'ivy)
      (org-roam-directory my-org-dir)
      (org-roam-directory my-org-dir)
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(defun my/org-roam ()
  (interactive)
  (let ((buf (current-buffer)))
    (my/org-roam-get-create-tab)
    (org-roam-buffer--get-create)
    (if (bound-and-true-p org-roam-mode)
        (pop-to-buffer buf))))

(defun my/org-roam-goto-todo ()
  (interactive)
  (my/org-roam-get-create-tab)
  (org-roam-buffer--get-create)
  (find-file my-org-todo-file)
  (org-roam-buffer--update-maybe :redisplay)
  (setq window (get-buffer-window org-roam-buffer))
  (if (window-live-p window)
      (select-window window)
    ))

(defun my/org-roam-get-create-tab ()
  (if (tab-bar--tab-index-by-name "roam")
      (tab-bar-switch-to-tab "roam")
    (tab-bar-new-tab-to 0)
    (tab-bar-rename-tab "roam"))
  )

(defun my/org-roam-new-tab ()
  (interactive)
  (if (tab-bar--tab-index-by-name "roam")
      (error "tab roam already exists"))
  (tab-bar-new-tab-to 0)
  (tab-bar-rename-tab "roam")
  (org-roam-mode 1)
  (org-roam))

(defun my/org-roam-find-file (&optional arg)
  (interactive "P")
  (if (and arg (not (eq 'visible (org-roam-buffer--visibility))))
      (org-roam))
  (org-roam-find-file))

(defun my/org-roam-get-titles ()
  (interactive)
  (let ((fn (if (eq major-mode 'dired-mode)
                (dired-get-filename)
              (buffer-file-name (or (buffer-base-buffer) (current-buffer)))))
        title)
    (if (not fn)
        (error "no org file is selected"))
    (setq title (org-roam--get-title-or-slug fn))
    (kill-new title)
    (message title)))

(provide 'init-org-roam)
