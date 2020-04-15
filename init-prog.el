(general-def 'normal 'prog-mode-map
  "<return>"     'xref-find-definitions-other-window
  "RET"          'xref-find-definitions-other-window
  "M-<up>"       'drag-stuff-up
  "M-<down>"     'drag-stuff-down
  "M-<left>"     'drag-stuff-left
  "M-<right>"    'drag-stuff-right
 )

(my-mc-def 'normal 'prog-mode-map
  "c"   'evil-commentary-line
  "e"   'vc-ediff
  "m"   'magit-blame
  "o"   'projectile-find-other-file
  )

(provide 'init-prog)
