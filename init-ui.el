;; for tumx run shell
(custom-set-faces
 '(ivy-prompt-match ((t (:inherit nil)))))

(face-spec-set 'line-number
  '((t (:foreground "RoyalBlue"))))

(face-spec-set 'line-number-current-line
  '((t (:foreground "firebrick"))))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(provide 'init-ui)
