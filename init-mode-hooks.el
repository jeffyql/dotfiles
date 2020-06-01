
;; (general-create-definer my-mc-def
;;   :prefix "mc"
;;   )

;; ;(general-create-definer my-md-def
;; ;  :prefix "md"
;; ;  )

;; ;(my-md-def '(normal motion visual) 'override
;; ;  "e"  'hydra-edit/body
;; ;  "f"  'hydra-file/body
;; ;  "v"  'hydra-misc/body
;; ;  )

;; (general-create-definer my-mf-def
;;   :prefix "mf"
;;   )

;; (general-create-definer my-mv-def
;;   :prefix "mv"
;;   )

;; ;; global misc commands
;; (my-mv-def
;;   :states '(normal motion visual)
;;   :keymaps 'override
;;   "b"   'my/buffer-to-file-or-new-buffer
;;   "f"   'magit-find-file
;;   "g"   'my/magit-status
;;   "h"   'highlight-symbol-at-point-all-windows
;;   "k"   'my/current-kill-to-file-or-new-buffer
;;   "p"   'my/add-or-remove-command-snippet
;;   "s"  (lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively 'deadgrep)))
;;   "t"   'tab-new
;;   "x"   'etcc-on
;;   )

;; (general-create-definer my-z-def
;;   :prefix "z")

;; (my-z-def '(normal motion visual) 'override
;;   "h"   'evil-scroll-left
;;   "l"   'evil-scroll-right
;;   )



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

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key   "a" 'dired-copy-filename-as-kill)
            (local-unset-key "b")
            (local-set-key   "c" 'goto-vterm)
            (local-set-key   "d" 'my/dired-up-directory)
            (local-unset-key "e")
            (local-unset-key "h")
            (local-set-key   "i" 'wdired-change-to-wdired-mode)
            (define-key evil-normal-state-local-map   "j" 'dired-next-line)
            (define-key evil-normal-state-local-map   "k" 'dired-previous-line)
            (local-unset-key "l")
            (local-unset-key "n")
            ;;(local-set-key "o" 'dired-find-file)
            (local-unset-key "p")
            (local-unset-key "q")
            (define-key evil-normal-state-local-map   "r" 'revert-buffer)
            (local-unset-key "t")
            (local-unset-key "v")
            (local-unset-key "w")
            (local-unset-key "y")
            (local-set-key "L" 'tmux-tail-this-file)
            (local-set-key "E" 'dired-toggle-read-only)
            ;;(local-set-key "O" 'dired-find-file-other-window)
            (local-set-key "T" 'dired-toggle-marks)
            (local-set-key "X" 'tmux-dired-run-file)
            (local-unset-key ",")
            (local-unset-key "'")
            (local-unset-key ".")
            (local-unset-key (kbd "DEL"))
            (local-unset-key (kbd "SPC"))
            ;; (define-key evil-normal-state-local-map   ";" 'avy-goto-word-or-subword-1)
            (define-key evil-normal-state-local-map  (kbd "<return>") 'dired-find-alternate-file)
            (local-set-key   "x" 'dired-mark)
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


(define-key org-mode-map (kbd "M-h") 'outline-up-heading)
(define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "M-l") 'org-goto-first-child-cmd)
(define-key org-mode-map (kbd "M-m") '(lambda () (interactive) (outline-back-to-heading)))
(define-key org-mode-map (kbd "M-RET") 'org-meta-return)

(define-key org-mode-map (kbd "<s-right>") 'org-metaright)
(define-key org-mode-map (kbd "<s-left>") 'org-metaleft)
(define-key org-mode-map (kbd "<s-up>") 'org-metaup)
(define-key org-mode-map (kbd "<s-down>") 'org-metadown)
(define-key org-mode-map (kbd "<S-s-right>") 'org-shiftmetaright)
(define-key org-mode-map (kbd "<S-s-left>") 'org-shiftmetarleft)
(define-key org-mode-map (kbd "<S-s-up>") 'org-metaup)
(define-key org-mode-map (kbd "<S-s-down>") 'org-metadown)
(define-key org-mode-map (kbd "<s-return>") 'org-meta-return)

(defun kill-ivy-file (x)
  (interactive)
  (kill-buffer x)
  (ivy--reset-state ivy-last)
    )

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

(add-hook 'rg-mode-hook
          (lambda ()
            (local-unset-key "f")
            (local-unset-key "g")
            ))

(add-hook 'ripgrep-search-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'compile-goto-error)))

(add-hook 'shell-mode-hook
          (lambda ()
            ;(define-key evil-normal-state-local-map [escape] 'my/comint-interrupt-subjob)
            (define-key shell-mode-map [escape] nil)
            ))

(add-hook 'xref--xref-buffer-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'xref-goto-xref)))


(provide 'init-mode-hooks)
