(use-package async :ensure t)

(use-package company
  :ensure t
  )

(use-package command-log-mode :ensure t)

(use-package deadgrep :ensure t)

(use-package elisp-def :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region))

(use-package flycheck
  :ensure t
  :init
  (progn
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'my-flycheck-fringe-indicator
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))

    (let ((bitmap 'my-flycheck-fringe-indicator))
      (flycheck-define-error-level 'error
        :severity 2
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-error)
      (flycheck-define-error-level 'warning
        :severity 1
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-warning)
      (flycheck-define-error-level 'info
        :severity 0
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap bitmap
        :fringe-face 'flycheck-fringe-info))

    ;; toggle flycheck window
    (defun spacemacs/toggle-flycheck-error-list ()
      "Toggle flycheck's error list window.
 If the error list is visible, hide it.  Otherwise, show it."
      (interactive)
      (-if-let (window (flycheck-get-error-list-window))
          (quit-window nil window)
        (flycheck-list-errors)))

    (defun spacemacs/goto-flycheck-error-list ()
      "Open and go to the error list buffer."
      (interactive)
      (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
        (flycheck-list-errors)
        (switch-to-buffer-other-window flycheck-error-list-buffer)))

    ))

(use-package flycheck-pos-tip
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))

(use-package flyspell-correct :ensure t)

(use-package flyspell-correct-ivy :ensure t)

(use-package general :ensure t)

;(use-package gruvbox-theme :ensure t)

(use-package outshine
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'outshine-mode)
  )

(use-package persistent-scratch
  :ensure t
  :config
  (progn
    (persistent-scratch-setup-default)
    ))

(use-package projectile
  :ensure t
  :config
  (progn
    (use-package counsel-projectile :ensure t)
    (counsel-projectile-mode 1)

    (defun my/counsel-projectile-switch-project-action-dired (project)
      "Open ‘dired’ at the root of the project."
      (let ((projectile-switch-project-action
	         (lambda ()
	           (projectile-dired))))
        (counsel-projectile-switch-project-by-name project)))

    (counsel-projectile-modify-action
     'counsel-projectile-switch-project-action
     '((add ("." my/counsel-projectile-switch-project-action-dired
	         "open ‘dired’ at the root of the project")
	        1)))

    (setq projectile-completion-system (quote ivy)
          projectile-generic-command "fd . -0"
          projectile-git-command "fd . -0")
    )
  )

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    ))

(use-package rg :ensure t)
(use-package ripgrep :ensure t)

(use-package subword
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'subword-mode)
    (add-hook 'ivy-occur-grep-mode-hook 'subword-mode))
  )

(use-package sudo-edit :ensure t)

(use-package tabbar :ensure t)

(use-package undo-tree :ensure t)

(use-package whitespace :ensure t)

(use-package winum :ensure t)

(use-package xterm-color :ensure t)

(provide 'init-packages)
