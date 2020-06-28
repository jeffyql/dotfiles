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
  "j"   'org-navigate-next/body
  "k"   'my/org-store-headline-link-1
  "h"   'my/org-store-headline-link
  "l"   'org-insert-link
  "m"   'org-mac-chrome-insert-frontmost-url
  "n"   'org-next-visible-heading
  "p"   'org-previous-visible-heading
  "s"  'my/org-store-headline-link
  "t"  'org-insert-structure-template
  "H"   'org-shiftmetaleft
  "K"  'org-metaup
  "L"  'org-shiftmetaright
  "o"  'my/org-open-link
  "i"  'my/org-open-link-this-window
  "y"  'org-cliplink
  "u"  'outline-up-heading
  "I"  'my/org-insert-prompt
  "U"  'org-babel-load-file
  "."  'org-time-stamp-inactive
  "&"   'org-mark-ring-goto
  "-"   'org-ctrl-c-minus
  "g"   'org-agenda-file-to-front
  "$"   'org-insert-prompt
  )

(my-mf-def
  :keymaps '(normal motion visual)
  "c"   'org-roam-db-build-cache
  "d"   'org-roam
  "f"   'org-roam-find-file
  "i"   'org-roam-insert
  "s"   'my/org-store-link-to-current-line
  "t"   'my/org-roam-new-tab
  )

(provide 'init-org-keybinding)
