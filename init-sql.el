(use-package sqlup-mode :ensure t)
(use-package sql-indent :ensure t)

(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-mode-hook 'sql-indent)


