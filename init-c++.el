(use-package ccls :ensure t)
  ;; :config
  ;; (progn
  ;;   (setq ccls-executable "/Users/jeff/ccls/Release/ccls")
;;   
          ;; ccls-args nil
          ;; projectile-project-root-files-top-down-recurring
          ;; (append '("compile_commands.json"
          ;;           ".ccls")
          ;;         projectile-project-root-files-top-down-recurring))))

(use-package google-c-style
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(provide 'init-c++)
