;; Keybindings

(general-def normal org-mode-map
  "RET"    'my/org-open-at-point
  )

(defun my/org-open-at-point ()
  (interactive)
  (let ((org-link-frame-setup
         (if (bound-and-true-p org-roam-backlinks-mode)
             '((file . find-file-other-window))
           '((file . find-file)))))
    (org-open-at-point)))

(my-mc-def
  :states 'normal
  :keymaps 'org-mode-map
  "$"   'org-archive-subtree
  "a"   'org-attach
  "b"   (lambda () (interactive) (outline-back-to-heading))
  "c"   'org-ctrl-c-ctrl-c
  "d"   'org-cut-special
  "e"   'org-export-dispatch
  "fd"  'my/org-bookmark-goto-definition
  "ff"  'my/org-goto-file-other-window
  "h"   'org-navigate-up/body
  "i"   'org-id-get-create
  "j"   'org-navigate-next/body
  "k"   'my/org-store-headline-link-1
  "h"   'my/org-store-headline-link
  "l"   'org-insert-link
  "m"   'org-mac-chrome-insert-frontmost-url
  "m"   'org-roam-node-insert
  "n"   'org-next-visible-heading
  "p"   'org-previous-visible-heading
  "l"   'org-roam-buffer-toggle
  "s"  'my/org-store-headline-link
  "H"   'org-shiftmetaleft
  "K"  'org-metaup
  "L"  'org-shiftmetaright
  "o"  'my/org-open-link
  "y"  'org-cliplink
  "u"  'outline-up-heading
  "I"  'my/org-insert-prompt
  "U"  'org-babel-load-file
  "."  'org-time-stamp-inactive
  "&"   'org-mark-ring-goto
  "-"   'org-ctrl-c-minus
  "g"   'org-agenda-file-to-front
  "$"   'org-insert-prompt
  ","   'org-insert-structure-template
  )

(my-mc-def
  :states 'normal
  :keymaps 'emacs-lisp-mode-map
  "c"   'evil-commentary-line
  "e"   'eval-defun
  )


;; (my-mf-def  'normal 'org-roam-mode-map
;;   "RET"   'org-roam-visit-thing
;;   "<return>"   'org-roam-visit-thing
;;   )

(provide 'init-org-keybinding)
