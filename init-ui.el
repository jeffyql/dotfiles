(if (eq system-type 'darwin)
    (progn
      (set-face-attribute 'default nil
                          :family "Iosevka"
                          :height 180
                          :background "cornsilk")
      (set-keyboard-coding-system 'iso-latin-2)
      (setq mac-command-key-is-meta t
            mac-command-modifier 'meta)
      )
  (set-face-attribute 'default nil
                      :height 120
                      :background "cornsilk")
  
  )

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
    (setq evil-insert-state-cursor '(box  "DarkOliveGreen3")
          evil-normal-state-cursor '(box "DarkGoldenrod2")  ;;
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

;; (use-package doom-modeline
;;   ;; :ensure t
;;   ;; :init (doom-modeline-mode 1)
;;   :config
;;   (setq-default doom-modeline-height 15)
  
;;   (defun my-doom-modeline--font-height ()
;;     "Calculate the actual char height of the mode-line."
;;     (+ (frame-char-height) 2))
  
;;   (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

;; (doom-modeline-def-modeline 'my-doom-modeline
;;   '(bar workspace-name window-number my-modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
;;   '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker time))

;; ;; Set default mode-line
;; (add-hook 'doom-modeline-mode-hook
;;           (lambda ()
;;             (doom-modeline-set-modeline 'my-doom-modeline 'default)))

;; )

(defvar my-selwin nil)

(defun my/get-selwin (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq my-selwin (selected-window))))

(add-function :before pre-redisplay-function #'my/get-selwin)


(setq-default mode-line-format
              (list
               '(:eval
                 (propertize (concat " %b %l " (make-string (window-body-width) ?\s)) 'face
                             (if (eq my-selwin (get-buffer-window))
                                 (if (and (buffer-modified-p) (not (eq major-mode 'vterm-mode)))
                                     (if (memq evil-state '(emacs insert))
                                         '(:background "DarkOliveGreen3" :foreground "red" :weight bold)
                                       '(:background "burlywood1" :foreground "red" :weight bold))
                                   (if (memq evil-state '(emacs insert))
                                       '(:background "DarkOliveGreen3" :foreground "black" :weight bold)
                                     '(:background "burlywood1" :foreground "black" :weight bold))))))))


(provide 'init-ui)
