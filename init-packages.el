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
    ;;(add-hook 'python-mode-hook 'flycheck-mode)
    ))

;; (use-package flycheck-pos-tip
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (flycheck-pos-tip-mode)))

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

(use-package treemacs
  :ensure t
  :init
  (setq treemacs-no-load-time-warnings t 
        treemacs-no-delete-other-windows nil)
  :config
  (progn
    (setq treemacs-no-png-images t)
    (set-face-attribute 'hl-line nil :background "#333333")
    ))

(use-package undo-tree :ensure t)

(use-package whitespace :ensure t)

(use-package winum :ensure t)

(use-package xterm-color :ensure t)

(use-package yasnippet :ensure t)

(provide 'init-packages)
