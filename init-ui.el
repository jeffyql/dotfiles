;; for tumx run shell
(custom-set-faces
 '(ivy-prompt-match ((t (:inherit nil)))))

(face-spec-set 'line-number
  '((t (:foreground "RoyalBlue"))))

(face-spec-set 'line-number-current-line
  '((t (:foreground "firebrick"))))

(use-package spaceline :ensure t)

(require 'spaceline-config)

(defun my/file-path ()
  (let ((name (buffer-file-name)))
    (when name
      (and (or (string-match "^.+\\(\\(/[^/]+\\)\\{3\\}\\)/[^/]+$" name)
               (string-match "^\\(.*/\\)[^/]+$" name))
           (concat "[" (substring (match-string 1 name) 1) "]")))))

(spaceline-define-segment file-name
  "Version control information."
  (my/file-path)
  )

(defun my/spaceline--theme (left second-left &rest additional-segments)
  "Convenience function for the spacemacs and emacs themes."
  (spaceline-compile
    `(,left
      (anzu :priority 95)
      auto-compile
     ; group-name
      ,second-left
      (file-name :priority 79)
      (process :when active)
      ((flycheck-error flycheck-warning flycheck-info)
       :when active
       :priority 89)
      (mu4e-alert-segment :when active)
      (erc-track :when active)
      (version-control :when active
                       :priority 78)
      (org-pomodoro :when active)
      (org-clock :when active)
      nyan-cat)
    `(which-function
      (python-pyvenv :fallback python-pyenv)
      (purpose :priority 94)
      (battery :when active)
      (selection-info :priority 95)
      input-method
      (global :when active)
      ,@additional-segments
      (buffer-position :priority 99)
      (hud :priority 99)))

  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
(defun my/spaceline-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.
ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (apply 'my/spaceline--theme
         '((persp-name
            workspace-number
            window-number)
           :fallback evil-state
           :face highlight-face
           :priority 100)
         '((buffer-modified buffer-size buffer-id)
           :priority 98)
         additional-segments))

(my/spaceline-theme)

(provide 'init-ui)
