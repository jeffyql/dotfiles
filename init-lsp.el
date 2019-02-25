(use-package company-lsp)

(use-package lsp-ui)

(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (require 'lsp-clients)
  (add-hook 'python-mode-hook 'lsp)
  (setq lsp-prefer-flymake nil))

(provide 'init-lsp)
