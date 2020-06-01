(use-package async :ensure t)

(use-package company
  :ensure t
  :config
  (define-key company-mode-map (kbd "M-j") 'company-select-next)
  (define-key company-mode-map (kbd "M-k") 'company-select-previous)
  (define-key company-mode-map (kbd "M-s") 'company-search-candidates)
  (add-hook 'python-mode-hook 'company-mode)
  )

(use-package command-log-mode :ensure t)

(use-package drag-stuff :ensure t)

(use-package elisp-def :ensure t)

;; (use-package flycheck-pos-tip
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (flycheck-pos-tip-mode)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package flyspell-correct :ensure t)

(use-package flyspell-correct-ivy :ensure t)

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  )

(use-package flycheck
  :ensure t
  :init
  (progn
    ;;(add-hook 'python-mode-hook 'flycheck-mode)
    ))

(use-package ivy-dired-history
  :ensure t
  :init
  (progn
    (savehist-mode 1)
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
    )
  )

(use-package magit
  :ensure t
  :config
  (progn
    (defun my/magit-status ()
      (interactive)
      (let ((pop-up-windows nil))
        (magit-status)))
    )
  )

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

(use-package subword
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'subword-mode)
    (add-hook 'ivy-occur-grep-mode-hook 'subword-mode))
  )

(use-package sudo-edit :ensure t)

;; (use-package treemacs
;;   :ensure t
;;   :init
;;   (setq treemacs-no-load-time-warnings t 
;;         treemacs-no-delete-other-windows nil)
;;   :config
;;   (progn
;;     (setq treemacs-no-png-images t)
;;     (set-face-attribute 'hl-line nil :background "#333333")
;;     ))

;; (use-package undo-tree :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

(use-package xterm-color :ensure t)

(use-package yasnippet :ensure t)

(provide 'init-packages)
