(use-package lsp-mode
  :hook ((c-mode c++-mode python-mode) . lsp)
  :init
  (setq lsp-prefer-flymake nil
        lsp-eldoc-enable-hover nil
        lsp-auto-guess-root t
        lsp-ui-sideline-enable nil)
  :config
  (require 'lsp-clients)
  )

(use-package lsp-ui
     :custom-face
     (lsp-ui-doc-background ((t `(:background nil))))
     :bind (:map lsp-ui-mode-map
                 ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                 ([remap xref-find-references] . lsp-ui-peek-find-references)
                 ("C-c u" . lsp-ui-imenu))
     :init
     (setq lsp-ui-doc-enable t
           lsp-ui-doc-include-signature t
           lsp-ui-doc-position 'at-point
           lsp-ui-doc-use-webkit t
           lsp-ui-doc-border (face-foreground 'default))
     )

(use-package company-lsp)
(provide 'init-lsp)
