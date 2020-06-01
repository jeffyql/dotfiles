
(add-hook 'compilation-mode-hook
          (lambda ()
            (local-unset-key "g")
            (define-key evil-normal-state-local-map (kbd "a") 'first-error)
            (define-key evil-normal-state-local-map (kbd "d") 'next-error)
            (define-key evil-normal-state-local-map (kbd "u") 'previous-error)
            (define-key evil-normal-state-local-map (kbd "M-j") 'compilation-next-error)
            (define-key evil-normal-state-local-map (kbd "M-k") 'compilation-previous-error)
            ))

(add-hook 'Custom-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'Custom-newline)
            ))

(add-hook 'diff-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map "j" 'diff-hunk-next)
            (define-key evil-normal-state-local-map "k" 'diff-hunk-prev)
            (define-key evil-normal-state-local-map "r" 'vc-refresh-state)
            (define-key evil-normal-state-local-map (kbd "RET") 'diff-goto-source)
            ))

(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (local-set-key [escape] 'ediff-quit)
            ;; (define-key ediff-mode-map "f" nil)
            ;; (define-key ediff-mode-map "g" nil)
            ;; (define-key ediff-mode-map "m" nil)
            (define-key evil-motion-state-local-map "q" 'ediff-quit)
            (define-key evil-motion-state-local-map "," 'my/select-window-or-tab)
            ))


(add-hook 'ivy-occur-mode-hook
          (lambda ()
            (local-unset-key "f")
            (local-unset-key "g")
            (local-unset-key "n")
            (local-unset-key "q")
            (local-unset-key "w")
            (local-set-key "o" 'ivy-occur-press)
            (local-set-key "r" 'ivy-occur-revert-buffer)
            (local-set-key ";" 'evil-avy-goto-subword-1)
            (define-key evil-normal-state-local-map (kbd "RET") 'ivy-occur-press-and-switch)
            ))

(add-hook 'ivy-occur-grep-mode-hook
          (lambda ()
            (local-unset-key "f")
            (local-unset-key "g")
            (local-unset-key "n")
            (local-unset-key "q")
            (local-unset-key "w")
            (local-set-key "o" 'ivy-occur-press)
            (local-set-key "r" 'ivy-occur-revert-buffer)
            (local-set-key ";" 'evil-avy-goto-subword-1)
            (local-set-key (kbd "RET") 'ivy-occur-press-and-switch)
            ))

(add-hook 'magit-status-mode-hook
          (lambda ()
            (local-unset-key "i")))

(add-hook 'Info-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'Info-follow-nearest-node)
            (define-key evil-normal-state-local-map "a" 'Info-history-back)
            (define-key evil-normal-state-local-map "i" 'Info-next)
            (define-key evil-normal-state-local-map "o" 'Info-prev)
            (define-key evil-normal-state-local-map "u" 'Info-up)
            ))

(add-hook 'tablist-mode-map-hook
          (lambda ()
            (define-key evil-normal-state-local-map "T" 'tablist-toggle-marks)
            (define-key evil-normal-state-local-map "." 'tablist-mark-forward)
            (define-key evil-normal-state-local-map "<" 'tablist-shrink-column)
            (define-key evil-normal-state-local-map ">" 'tablist-enlarge-column)
            (define-key evil-normal-state-local-map "s" 'tablist-sort)
            (define-key evil-normal-state-local-map "u" 'tablist-unmark-forward)
            ))

(add-hook 'shell-mode-hook
          (lambda ()
            ;(define-key evil-normal-state-local-map [escape] 'my/comint-interrupt-subjob)
            (define-key shell-mode-map [escape] nil)
            ))

(add-hook 'xref--xref-buffer-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'xref-goto-xref)))


(provide 'init-mode-hooks)
