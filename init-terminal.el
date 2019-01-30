(setq interprogram-cut-function 'osc52-select-text-tmux)
(require 'osc52e)

(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :demand
  :init
  (setq evil-visual-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-emacs-state-cursor 'hbar)
  :config
  (evil-terminal-cursor-changer-activate)

  ;; (setq-default filter-buffer-substring-function
  ;;               ;; #'buffer-substring--filter
  ;;               #'abn-tty/buffer-substring-terminal-filter
  ;;               )
  )

(provide 'init-terminal)
