(package-install 'julia-mode)
(require 'julia-mode)

(use-package julia-vterm :ensure t
  :config
  (add-hook 'julia-mode-hook #'julia-vterm-mode)
  (setq julia-vterm-repl-program "/usr/local/bin/julia -t 4")
  )

(use-package julia-repl :ensure t)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

(setenv "JULIA_NUM_THREADS" "4")
(julia-repl-set-terminal-backend 'vterm)
(define-key julia-repl-mode-map (kbd "<M-RET>") 'my/julia-repl-send-cell)
(define-key julia-repl-mode-map (kbd "<C-RET>") 'julia-repl-send-line)
(define-key julia-repl-mode-map (kbd "<S-return>") 'julia-repl-send-buffer)


(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.8")
  (add-hook 'julia-mode-hook #'lsp-mode)
  )

(my-mc-def 'normal 'julia-mode-map
  "b"   'julia-repl-send-buffer
  "c"   'my/comment-or-add-cell
  "d"   'julia-repl-doc
  )

(defun my/comment-or-add-cell ()
  (interactive)
  (if (and (line-beginning-position) (looking-at-p "[[:blank:]]*$") (not (use-region-p)))
      (progn
        (insert "###\n\n###")
        (forward-line -1))
    (funcall 'comment-line 1)))

(defun my/julia-repl-send-cell() 
  ;; "Send the current julia cell (delimited by ###) to the julia shell"
  (interactive)
  (save-excursion (setq cell-begin (if (re-search-backward "^###" nil t)  (progn (forward-line 1) (point)) (point-min))))
  (save-excursion (setq cell-end (if (re-search-forward "^###" nil t) (progn (forward-line -1) (point)) (point-max))))
  (set-mark cell-begin)
  (goto-char cell-end)
  (julia-repl-send-region-or-line)
  (next-line))

(evil-add-command-properties #'my/julia-repl-send-cell :jump t)
(require 'eglot-jl)
(eglot-jl-init)
(provide 'init-julia)
