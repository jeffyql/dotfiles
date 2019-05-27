(use-package lsp-mode
  :hook ((c-mode
          c++-mode
          python-mode
          java-mode
          ) . lsp)
  :init
  (setq
   lsp-prefer-flymake nil
        lsp-eldoc-enable-hover nil
        lsp-auto-guess-root t
        lsp-ui-sideline-enable nil)
  )
;;lsp-ui-imenu-mode-map
(use-package lsp-ui
     :bind (:map lsp-ui-mode-map
                 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                 ([remap xref-find-references] . lsp-ui-peek-find-references)
                 ("C-c u" . lsp-ui-imenu))
     :config
     (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
     (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
     )

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package company-lsp)
(provide 'init-ide)
