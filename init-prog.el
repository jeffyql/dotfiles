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
  ;; "m"   'magit-blame
  "m"   'code-cells-insert-boundary
  "o"   'projectile-find-other-file
  )

(add-hook 'prog-mode-hook (lambda () (interactive)
                             (display-line-numbers-mode)
                             (setq display-line-numbers-widen t)))

(use-package code-cells
  :ensure t
  )

(defun toggle-code-cells-mode ()
  (interactive)
  (if code-cells-mode
      (code-cells-mode -1)
    (setq code-cells-boundary-regexp
          (cond
           ((eq major-mode 'python-mode)
            "\\s<+\\(?:\\s-*%\\(?1:%+\\)\\|\\(?1:\\*+\\)\\| In\\[[[:space:][:digit:]]*]:\\)")      
           ((eq major-mode 'sql-mode)
            "^-- %%")
           (t nil)
           ))
    (make-local-variable 'code-cells-boundary-regexp)
    (code-cells-mode 1)
    ))

(defun code-cells-insert-boundary ()
  (interactive)
  (end-of-line)
  (if (save-excursion
        (beginning-of-line)
        (looking-at-p "[[:blank:]]*$"))
      (progn
        (open-line 1)
        (next-line 1))
    (open-line 2)
    (next-line 2))
  (cond
   ((eq major-mode 'python-mode)
    (insert "# %%\n"))
   ((eq major-mode 'sql-mode)
    (insert "-- %% \n"))
   )
  )

(provide 'init-prog)
