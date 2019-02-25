(my-local-leader-def
  :states 'normal
  :keymaps 'emacs-lisp-mode-map
  "e"   'eval-defun
  "ESC" 'keyboard-quit
  )

(provide 'init-elisp)
