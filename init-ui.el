(when (eq system-type 'darwin)
  (set-default-font "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq-default ring-bell-function 'ignore)

;; (use-package ivy-posframe
;;   :ensure t
;;   :config
;;   (progn
;;     (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;;     ))
;(ivy-posframe-enable)
; for tumx run sf.hell
(custom-set-faces
 '(ivy-posframe ((t (:inherit default))))
 '(ivy-prompt-match ((t (:inherit nil)))))

(face-spec-set 'line-number
  '((t (:foreground "RoyalBlue"))))

(face-spec-set 'line-number-current-line
  '((t (:foreground "firebrick"))))

(use-package doom-modeline
      :ensure t
      :init
      (progn
        (require 'ace-window)
        (setq doom-modeline-env-enable-python t
              doom-modeline-height 22)
        )
      :hook (after-init . doom-modeline-mode))

(provide 'init-ui)
