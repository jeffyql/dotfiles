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

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(blink-cursor-mode 0)

(tab-bar-mode)
(setq tab-bar-show 1)

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

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package doom-modeline
  :ensure t
  :init
    (setq doom-modeline-modal-icon t)
  ;; :custom-face
  ;; (mode-line ((t (:height 1.0))))
  ;; (mode-line-inactive ((t (:height 1.0))))
  :config
  (progn

  ;;   (setq doom-modeline-buffer-file-name-style 'relative-from-project
  ;;         doom-one-brighter-modeline t)
  ;;   (set-face-foreground 'doom-modeline-evil-emacs-state "SkyBlue2")
  ;;   (set-face-foreground 'doom-modeline-evil-insert-state "chartreuse3")
  ;;   (set-face-foreground 'doom-modeline-evil-motion-state "plum3")
  ;;   (set-face-foreground 'doom-modeline-evil-normal-state "DarkGoldenrod2")
  ;;   (set-face-foreground 'doom-modeline-evil-operator-state "DarkGoldenrod2")
  ;;   (set-face-foreground 'doom-modeline-evil-visual-state "gray")
  ;;   (set-face-foreground 'doom-modeline-evil-replace-state "chocolate")
  ;;   (doom-modeline-init)
    )
  :hook (after-init . doom-modeline-mode))

;; (defun my-doom-modeline--font-height ()
;;   "Calculate the actual char height of the mode-line."
;;   (+ (frame-char-height) 2))
;; (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

;; (use-package doom-modeline
;;   :ensure t
;;   :config
;;   (progn
;;     (setq doom-modeline-buffer-file-name-style 'file-name)
;;     (set-face-foreground 'doom-modeline-evil-emacs-state "SkyBlue2")
;;     (set-face-foreground 'doom-modeline-evil-insert-state "chartreuse3")
;;     (set-face-foreground 'doom-modeline-evil-motion-state "plum3")
;;     (set-face-foreground 'doom-modeline-evil-normal-state "DarkGoldenrod2")
;;     (set-face-foreground 'doom-modeline-evil-operator-state "DarkGoldenrod2")
;;     (set-face-foreground 'doom-modeline-evil-visual-state "gray")
;;     (set-face-foreground 'doom-modeline-evil-replace-state "chocolate")
;;     )
;;   :hook (after-init . doom-modeline-mode))

(provide 'init-ui)
