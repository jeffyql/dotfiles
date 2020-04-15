(general-define-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 "RET" 'xref-find-definitions-other-window
 )

(my-mc-def
  :states 'normal
  :keymaps 'emacs-lisp-mode-map
  "c"   'evil-commentary-line
  "e"   'eval-defun
  )

(provide 'init-elisp)
