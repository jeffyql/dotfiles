(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-search-module (quote evil-search)
          evil-symbol-word-search t)
                                        ; (setq evil-ex-search-persistent-highlight nil)
    (setq evil-insert-state-cursor '((bar . 2) "chartreuse3")
          evil-normal-state-cursor '(box "DarkGoldenrod2")
          evil-emacs-state-cursor '(box "SkyBlue2"))
    (setq evil-shift-round nil
          evil-move-beyond-eol t)
    (setq evil-motion-state-modes nil)
    (setq evil-shift-width 1)
    (evil-mode 1)

    (define-key evil-normal-state-map [escape] 'my/keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)

    (add-hook 'dired-mode-hook
              (lambda ()
                (local-set-key   "a" 'dired-copy-filename-as-kill)
                (local-unset-key "b")
                (local-set-key   "c" 'goto-vterm)
                (local-set-key   "d" 'my/dired-up-directory)
                (local-unset-key "e")
                (local-unset-key "f")
                (local-unset-key "h")
                (local-set-key   "i" 'wdired-change-to-wdired-mode)
                (define-key evil-normal-state-local-map   "j" 'dired-next-line)
                (define-key evil-normal-state-local-map   "k" 'dired-previous-line)
                (local-unset-key "l")
                (local-unset-key "m")
                (local-unset-key "n")
                ;;(local-set-key "o" 'dired-find-file)
                (local-unset-key "p")
                (local-unset-key "q")
                (define-key evil-normal-state-local-map   "r" 'revert-buffer)
                (local-unset-key "s")
                (local-unset-key "t")
                (local-unset-key "v")
                (local-unset-key "w")
                (local-unset-key "y")
                (local-set-key "E" 'dired-toggle-read-only)
                ;;(local-set-key "O" 'dired-find-file-other-window)
                (local-set-key "T" 'dired-toggle-marks)
                (local-set-key "X" 'tmux-dired-run-file)
                (local-unset-key ",")
                (local-unset-key "'")
                (local-unset-key ".")
                (local-unset-key (kbd "DEL"))
                (local-unset-key (kbd "SPC"))
                (define-key evil-normal-state-local-map   ";" 'avy-goto-word-or-subword-1)
                ;; (define-key evil-normal-state-local-map  (kbd "<return>") 'dired-find-alternate-file)
                (local-set-key   "x" 'dired-mark)
                ))

    (use-package evil-commentary :ensure t)
    (use-package evil-ediff :ensure t)
    (use-package evil-matchit :ensure t)
    (use-package evil-surround
      :ensure t
      :config
      (global-evil-surround-mode 1)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region))
    )

  (defun my/keyboard-quit ()
    (interactive)
    (if (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-delete-hl 'evil-ex-search))
    (keyboard-quit))

  (defun my/evil-insert ()
    (interactive)
    (if (memq major-mode evil-emacs-state-modes)
        (evil-emacs-state 1)
      (call-interactively 'evil-insert)))
)

(use-package general :ensure t)

(general-def
  :keymaps  '(motion normal visual)
  ";"   'avy-goto-word-1
  ","   'my/select-tab-or-toggle-buffer
  "'"   'my/toggle-buffer
  "p"   'my/evil-paste-after
  "q"   'my/kill-this-buffer
  "s"   'my/select-window
  "t"   'universal-argument
  "DEL" 'my/evil-scroll-up
  "SPC" 'my/evil-scroll-down
  "TAB" nil
 )

(general-def 'visual
 "a"  'mark-whole-buffer
 "i"  'evil-visual-line
 "o"  'evil-visual-block
 "r"  'evil-surround-region
 "u"  'er/contract-region
 "v"  'er/expand-region
 )

(define-key universal-argument-map "t" 'universal-argument-more)

(general-def
 "M-i" 'completion-at-point
 "M-." 'xref-find-definitions
 "M-?" 'xref-find-references
 "<s-right>" nil
 "<s-left>" nil
 "<s-up>" nil
 "<s-down>" nil
 )

(evil-define-key nil evil-normal-state-map "f" nil)
(general-create-definer my-f-def
  :prefix "f")

(my-f-def
  :keymaps 'normal
  "ESC"  'keyboard-quit
  "a"    'my/recentf-misc
  "d"    'dired
  "e"    'my/recentf-el
  "f"    'counsel-find-file
  "g"    'grep
  "h"    'hydra-help/body
  "i"    'counsel-imenu
  "j"    'my/recentf-main-language-by-project
  "l"    'my/counsel-rg
  "k"    'my/counsel-rg-at-point
  "m"    'counsel-grep-or-swiper
  "n"    'my/swiper-symbol
  "p"    'my/projectile-find-file
  "r"    'my/counsel-rg-org-search
  "s"    'ivy-switch-buffer
  "t"    'my/ripgrep-this-file
  "u"    'counsel-bookmark
  "v"    'my/counsel-narrowed-indirect
  "w"    'tmux-select-active-window
  "y"    'counsel-yank-pop
  ","    'my/swiper-current-kill
  ";"    'my/counsel-rg-current-kill
  "."    'ivy-resume
  "7"    'select-vterm-7
  "8"    'select-vterm-8
  "9"    'select-vterm-9
  "0"    'select-vterm-0
  )

(evil-define-key nil evil-normal-state-map "g" nil)
(general-create-definer my-g-def
  :prefix "g")

(my-g-def
  :keymaps '(normal motion visual) 
  "ESC"  'keyboard-quit
  "a"    'evil-first-non-blank
  "b"    (lambda () (interactive) (move-to-window-line -1))
  "c"    'my/last-log-file
  "d"    'dired-jump
  "e"    'evil-last-non-blank
  "f"    'firefox-to-front
  "g"    'evil-goto-first-line
  "h"    (lambda () (interactive) (dired "~"))
  "i"    'my/goto-indirect-narrow-at-point
  "j"    'my/open-last-main-language-file
  "k"    'hydra-last-buffer-by-mode/body
  "l"    'evil-goto-line
  "m"    'my/open-last-misc-file
  "n"    'select-vterm
  "o"    'my/open-last-org-file
  "p"    'my/projectile-dired
  "s"    '(lambda () (interactive) (switch-to-buffer "*scratch*"))
  "t"    (lambda () (interactive) (move-to-window-line 0))
  ";"    'goto-last-change
  ","    'pop-to-mark-command
  "."    'evil-scroll-line-to-center
  "G"    (lambda () (interactive) (find-file my-org-checklist-file))
  "SPC"  'my/org-roam-goto-todo
  "RET"  'hydra-org-cycle-agenda-files/body
  )

(evil-define-key nil evil-normal-state-map "m" nil)
(general-create-definer my-m-def
  :prefix "m")

(my-m-def
  :keymaps '(normal motion visual) 
  "a"    'my/kill-ring-save-symbol-at-point
  "b"    'hydra-buffer/body
  "c"    nil    ;;c key is reserved as major mode leader 
  "e"    nil
  "d"    'my/send-command
  "f"    nil
  "g"    nil
  "i"    'my/switch-indirect-narrow
  "h"    'hydra-move-window-splitter-left-or-up/body
  "j"    'hydra-scroll-line-up/body
  "k"    'hydra-scroll-line-down/body
  "l"    'hydra-move-window-splitter-right-or-down/body
  "m"    'my/save-buffer
  "n"    'my/evil-ex-search-word-forward
  "o"    'occur
  "q"    'my/delete-or-split-window
  "r"    'my/open-next-line
  "s"    'my/occur-at-point
  "t"    'hydra-toggle/body
  "u"    'hydra-kmacro-end-or-call-macro-repeat/body
  "w"    'my/swap-window
  "x"    'counsel-M-x
  "v"    nil
  "z"    'my/toggle-or-split-window
  "B"    'balance-windows
  "D"    'save-buffers-kill-emacs
  "G"    'magit-dispatch-popup
  "J"    'move-line-down
  "K"    'move-line-up
  "T"    'test
  "X"    'shell-command
  ","    'my/org-store-link-to-current-line
  "ESC"  'keyboard-quit
  "SPC"  'show-terminal
  "U"    'kmacro-start-macro
  "1"    'test
  "1"    'vterm-by-number
  "2"    'vterm-by-number
  "3"    'vterm-by-number
  "4"    'vterm-by-number
  "5"    'vterm-by-number
  "6"    'vterm-by-number
  )

(general-create-definer my-mc-def
  :prefix "mc"
  )

;(general-create-definer my-md-def
;  :prefix "md"
;  )

;(my-md-def '(normal motion visual) 'override
;  "e"  'hydra-edit/body
;  "f"  'hydra-file/body
;  "v"  'hydra-misc/body
;  )

(general-create-definer my-mf-def
  :prefix "mf"
  )

(general-create-definer my-mv-def
  :prefix "mv"
  )

;; global misc commands
(my-mv-def
  :keymaps '(normal motion visual)
  "b"   'my/buffer-to-file-or-new-buffer
  "f"   'magit-find-file
  "g"   'my/magit-status
  "h"   'highlight-symbol-at-point-all-windows
  "k"   'my/current-kill-to-file-or-new-buffer
  "p"   'my/add-or-remove-command-snippet
  "s"  (lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively 'deadgrep)))
  "t"   'tab-new
  "x"   'etcc-on
  )

(evil-define-key nil evil-normal-state-map "z" nil)
(general-create-definer my-z-def
  :prefix "z")

(my-z-def
  :keymaps '(normal motion visual)
  "h"   'evil-scroll-left
  "l"   'evil-scroll-right
  )

;;;; enable escape key
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; (with-eval-after-load "helm"
;;   (define-key helm-map  [escape] 'keyboard-escape-quit))
(with-eval-after-load "ivy"
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))
;; (with-eval-after-load "popup"
;;   (define-key popup-menu-keymap  [escape] 'keyboard-quit))

(provide 'init-keybindings)

