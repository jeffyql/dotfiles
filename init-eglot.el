(use-package eglot :ensure t)
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
;; (add-to-list 'eglot-server-programs '((python-mode) "/Users/yuanqianli/miniconda3/bin/pyright-langserver" "--stdio"))
;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-10"))
;; (add-hook 'c++-mode-hook 'eglot-ensure)

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(provide 'init-eglot)
