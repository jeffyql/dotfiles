(use-package doom-modeline
      :ensure t
      :config
      (progn
        (setq doom-modeline-buffer-file-name-style 'relative-from-project
              doom-modeline-height 20
              doom-modeline-bar-width 2)
        (setq doom-one-brighter-modeline t)
        (doom-modeline-init)
        )
      :hook (after-init . doom-modeline-mode))

(setq doom-modeline-height 1)
(set-face-attribute 'mode-line nil :family "Hack" :height 140)
(set-face-attribute 'mode-line-inactive nil :family "Hack" :height 140)

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil
                      :family "Hack"
                      :height 150
                      :background "cornsilk")
  (set-keyboard-coding-system 'iso-latin-2)

  (setq mac-command-key-is-meta t
        mac-command-modifier 'meta)
  )

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(blink-cursor-mode 0)

(setq-default ring-bell-function 'ignore)

(set-face-attribute 'org-checkbox-statistics-todo nil :inherit 'link)

;; display-line-numbers-mode
(face-spec-set 'line-number
  '((t (:foreground "RoyalBlue"))))
(face-spec-set 'line-number-current-line
  '((t (:foreground "firebrick"))))
(setq display-line-numbers-widen t)
(global-display-line-numbers-mode 1)

;; (use-package winum
;;   :ensure t
;;   :config
;;   ;; (defun my/show-frame-winum (frame)
;;   ;;  (winum-get-number-string (frame-first-window frame)))
;;   (progn
;;     (winum-mode))
;;   ;  (add-hook 'after-make-frame-functions 'my/show-frame-winum)
;;   )

;; (defun anders/same-window-instead
;;     (orig-fun buffer alist)
;;   (display-buffer-same-window buffer nil))
;; (advice-add 'display-buffer-pop-up-window :around 'anders/same-window-instead)

;; (setq display-buffer--other-frame-action
;;   '((display-buffer-reuse-window
;;      display-buffer-pop-up-frame)
;;     (reusable-frames . 1)
;;     (inhibit-same-window . nil)))
 

(provide 'init-ui)
