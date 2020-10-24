(if (eq system-type 'darwin)
    (progn
      (set-face-attribute 'default nil
                          :family "Hack"
                          :height 140
                          :background "cornsilk")
      (set-keyboard-coding-system 'iso-latin-2)
      (setq mac-command-key-is-meta t
            mac-command-modifier 'meta)
      )
  (set-face-attribute 'default nil
                      :height 120
                      :background "cornsilk")
  
  )

(set-face-attribute 'ivy-prompt-match nil :inherit nil)
(set-face-attribute 'org-checkbox-statistics-todo nil :inherit 'link)

(menu-bar-mode -1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(blink-cursor-mode 0)

;; (use-package tabbar :ensure t)

;; (tab-bar-mode)
;; (setq tab-bar-show 1)

(setq line-number-display-limit-width 2000000
      display-line-numbers-widen t)

;; display-line-numbers-mode
(face-spec-set 'line-number
  '((t (:foreground "RoyalBlue"))))
(face-spec-set 'line-number-current-line
  '((t (:foreground "firebrick"))))
(add-hook 'prog-mode-hook
  (lambda () (display-line-numbers-mode 1)))

;; Reuse windows as default
(setq pop-up-windows nil)

(if (display-graphic-p)
    (setq evil-insert-state-cursor '(hbar  "chartreuse3")
          evil-normal-state-cursor '(box "DarkGoldenrod2")
          evil-emacs-state-cursor '(hbar "SkyBlue2"))
  (setq evil-visual-state-cursor 'box
        evil-normal-state-cursor 'box
	    evil-insert-state-cursor 'hbar
	    evil-emacs-state-cursor 'hbar)
  )

(setq-default ring-bell-function 'ignore)

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    ))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package doom-modeline

      :config
      (progn
        (setq doom-modeline-buffer-file-name-style 'relative-from-project
              doom-modeline-height 8)
        (setq doom-one-brighter-modeline t)
        (doom-modeline-init)
        (remove-hook 'after-change-functions #'doom-modeline-update-buffer-file-name-face)
        (add-function :after after-focus-change-function #'my/doom-modeline-update-face)
        (add-hook 'after-change-functions #'my/doom-modeline-normal-state-after-change-set-face)
        (add-hook 'evil-insert-state-entry-hook #'my/doom-modeline-insert-state-update-face)
        ;;(add-hook 'after-change-major-mode-hook #'my/doom-modeline-update-face)
        )
      :hook (after-init . doom-modeline-mode))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defun my/doom-modeline-update-face ()
  (if (frame-focus-state)
      (setq doom-modeline--buffer-file-name
            (cond
             ((evil-insert-state-p)
              (propertize doom-modeline--buffer-file-name
                          'face 'widget-single-line-field))
             ((and (equal major-mode 'vterm-mode) vterm-copy-mode)
              (propertize doom-modeline--buffer-file-name
                          'face 'doom-modeline-debug-visual))
             ((and (buffer-modified-p) (not (equal major-mode 'vterm-mode)))
              (propertize doom-modeline--buffer-file-name
                          'face 'doom-modeline-buffer-modified))
             (t
              (propertize "%b"
                          'face 'doom-modeline-buffer-file
                          'mouse-face 'mode-line-highlight
                          'help-echo "Buffer name mouse-1: Previous buffer\nmouse-3: Next buffer"
                          'local-map mode-line-buffer-identification-keymap))))))

(defun my/doom-modeline-insert-state-update-face ()
  (setq doom-modeline--buffer-file-name
        (propertize doom-modeline--buffer-file-name
                    'face 'widget-single-line-field)))

(defun my/doom-modeline-normal-state-after-change-set-face (&rest _)
  (when (and (evil-normal-state-p)
             (not (equal major-mode 'vterm-mode))
             doom-modeline--buffer-file-name
             (buffer-modified-p))
    (setq doom-modeline--buffer-file-name
          (propertize doom-modeline--buffer-file-name
                      'face 'doom-modeline-buffer-modified))))

(provide 'init-ui)
