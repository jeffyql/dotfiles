;; -*- lexical-binding: t -*-

;;; better default
(setq debug-on-error t)

(add-hook 'after-init-hook
          (lambda () (setq debug-on-error nil)))

(setq bookmark-default-file "~/db/bookmarks")

(require 'dired-x)
(require 'ediff)
(require 'paren)
(require 'which-func)
(require 'winner)

(setq custom-file "~/.emacs-custom.el")

(electric-pair-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(blink-cursor-mode 0)

(delete-selection-mode 1)

;; highlight the current line
(setq-default   auto-revert-verbose nil
                bidi-display-reordering nil
                column-number-mode t
                confirm-kill-emacs #'y-or-n-p
                dired-dwim-target t
                dired-recursive-copies (quote always)
                dired-listing-switches "-alt"
                enable-recursive-minibuffers t
                inhibit-startup-screen t
                initial-scratch-message ""
                indent-tabs-mode nil
                large-file-warning-threshold 100000000
                recentf-max-saved-items 500
                require-final-newline t
                tab-always-indent 'complete
                tab-width 4
                ring-bell-function 'ignore
                )

(setq imenu-auto-rescan t
      imenu-auto-rescan-maxout 600000
      imenu-max-items 250)

(setq ediff-diff-options "-w")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(recentf-mode 1)

(defsubst file-was-visible-p (file)
  "Return non-nil if FILE's buffer exists and has been displayed."
  (let ((buf (find-buffer-visiting file)))
    (if buf
        (let ((display-count (buffer-local-value 'buffer-display-count buf)))
          (if (> display-count 0) display-count nil)))))

(let ((r-list recentf-list))
  (defsubst keep-default-old-and-visible-recentf-p (file)
    "Decide whether to keep file in recentf-list.
Return non-nil if recentf would, by default, keep FILE, and
either FILE name was loaded from recentf file on disk or FILE
has been displayed in this session."
    (if (recentf-keep-default-predicate file)
      (or (member file r-list)
      (file-was-visible-p file)))))

(setf recentf-keep '(keep-default-old-and-visible-recentf-p))

(add-hook 'buffer-list-update-hook 'recentf-track-opened-file)

(show-paren-mode)

;; activate it for all buffers
(save-place-mode)

(savehist-mode 1)

;; tramp needs a clean recognizable prompt on the remote host for accurate parsing.
;; Shell prompts that contain escape sequences for coloring cause parsing problems.
;; Remote shell setup for customizing prompt detection using regular expressions.

;; To check if the remote host's prompt is being recognized, use this test:
;; switch to tramp connection buffer *tramp/foo*, put the cursor at the top of the buffer,
;; and then apply the following expression:
;; M-: (re-search-forward (concat tramp-shell-prompt-pattern "$"))
;; If the cursor has not moved to the prompt at the bottom of the buffer,
;; then tramp has failed to recognize the prompt.
(setq tramp-shell-prompt-pattern  "\\(?:^\\|^M\\).*[$] *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(setq tramp-verbose 2)
(setq password-cache-expiry nil)
(setq remote-file-name-inhibit-cache nil)
(setq tramp-completion-reread-directory-timeout nil)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;(setq tramp-histfile-override "/tmp/.tramp_history")
(setq tramp-histfile-override "/dev/null")

(add-hook 'diff-mode-hook 'read-only-mode)

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;(setq scroll-margin 0
;      scroll-conservatively 100000
;      scroll-preserve-screen-position 1)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/backup"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(add-hook 'prog-mode-hook (lambda () (interactive)
                             (display-line-numbers-mode)
                             (setq display-line-numbers-widen t)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; enable reuse directory buffer
(put 'dired-find-alternate-file 'disabled nil)


(use-package avy
  :ensure t
  :config
  (setq avy-background nil
        avy-all-windows (quote all-frames))
  )

(use-package counsel :ensure t)

(use-package deadgrep :ensure t)

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
    (use-package evil-commentary :ensure t)
    (use-package evil-ediff :ensure t)
    (use-package evil-matchit :ensure t)
    )
)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region))

(use-package expand-region :ensure t)

(use-package flycheck
  :ensure t
  :init
  (progn
    ;;(add-hook 'python-mode-hook 'flycheck-mode)
    ))

(use-package general :ensure t)

(use-package hydra :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (progn
    ;(ivy-mode 1)
    ;;(setq ivy-use-virtual-buffers t)
    (setq ivy-use-selectable-prompt t
          )
    (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
    (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
    )
  )

(use-package ivy-dired-history
  :ensure t
  :init
  (progn
    (savehist-mode 1)
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
    )
  )

(use-package magit
  :ensure t
  :config
  (progn
    (defun my/magit-status ()
      (interactive)
      (let ((pop-up-windows nil))
        (magit-status)))
    )
  )

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(use-package org
  :ensure org-plus-contrib
  :pin org)

(use-package outshine
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'outshine-mode)
  )

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :custom
  (projectile-current-project-on-switch 'keep)
  (projectile-completion-system 'ivy)
  (projectile-generic-command "fd . -0")
  (projectile-git-command "fd . -0")
  )
;; (use-package projectile
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map))
;; (use-package counsel-projectile
;;  :after (counsel projectile)
;;  :config
;;  (counsel-projectile-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    ))

(use-package shackle
  :ensure t
  :config
  (setq shackle-default-rule '(:same t))
  (shackle-mode 1))

(use-package undo-tree :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

(use-package winum
  :ensure t
  :config
  (winum-mode))

;;; editing

(defun my/kill-ring-save-symbol-at-point ()
  "Kill word under cursor"
  (interactive)
  (let (symbol)
    (if (symbol-at-point)
        (progn
          (setq symbol (thing-at-point 'symbol))
          (kill-new symbol)
          (message "%s" symbol))
      (message "no symbol is under the cursor"))))

(defun my/evil-scroll-down (&optional arg)
  (interactive "P")
  (if arg
      (evil-scroll-down 0)
    (evil-scroll-page-down 1)
  ))

(defun my/evil-scroll-up (&optional arg)
  (interactive "P")
  (if arg
      (evil-scroll-up 0)
    (evil-scroll-page-up 1)
  ))

(defun my/evil-insert ()
  (interactive)
  (if (memq major-mode evil-emacs-state-modes)
      (evil-emacs-state 1)
    (call-interactively 'evil-insert)))

(defun my/dired-up-directory ()
  (interactive)
  (let ((buf (current-buffer)))
    (dired-up-directory)
    (kill-buffer buf)))

(defun my/evil-ex-search-word-forward ()
  (interactive)
  (save-excursion
    (call-interactively 'evil-ex-search-word-forward))
  (beginning-of-thing 'symbol))

(defun my/toggle-buffer ()
  "Toggle buffers, ignoring certain ones."
  (interactive)
  (catch 'done
    (cl-loop for buf in (buffer-list) do
             (unless (or (equal (current-buffer) buf)
                         (eq (window-buffer (next-window (selected-window) nil 'visible)) buf)
                         (string-match "^ +\\*" (buffer-name buf)))
                 (switch-to-buffer buf)
                 (throw 'done t)))
    (message "no more candidate buffer")))

;;; buffer

(defun my/kill-this-buffer ()
   (interactive)
   (if (or (equal "*scratch*" (buffer-name))
           (equal "*Messages*" (buffer-name)))
       (error "buffer is reserved"))
   (if (buffer-modified-p)
       (error "buffer is modified"))
     (kill-this-buffer))

(defun my/save-buffer (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (save-some-buffers)
      (call-interactively 'save-buffer)))

;;; search/navigation
;;; frame tab and window

(defun my/select-tab-or-toggle-buffer (&optional arg)
   (interactive "P")
   (if (integerp arg)
       (tab-bar-select-tab arg)
     (if arg
         (tab-bar-switch-to-recent-tab)
       (my/toggle-buffer))))
 
 (defun my/select-window ()
   (interactive)
   (let ((next-window (next-window)))
     (unless (eq next-window (selected-window))
       (select-window next-window)))
   (run-hooks 'window-configuration-change-hook))

(defun my/delete-or-split-window ()
  (interactive)
  (if (< 1 (count-windows))
      (delete-other-windows)
    (split-window-right)
    (my/toggle-buffer)))

(defun my/toggle-or-split-window ()
  (interactive)
  (if (= 1 (count-windows))
      (progn
        (split-window-below)
        (my/toggle-buffer))
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1)))))))

(defun my/next-frame ()
  (interactive)
  (let ((next-frame
         (if (<= (length (frame-list)) 2)
             (make-frame)
           (next-frame))))
    (select-frame-set-input-focus next-frame)
    (run-hooks 'window-configuration-change-hook)))
 
(defun my/delete-window (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (delete-window)
    (delete-other-windows)))

(defun my/split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (select-window (next-window))
  (switch-to-buffer (other-buffer))
  )

(defun my/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (select-window (next-window))
  (switch-to-buffer (other-buffer))
  )

(defun my/split-window (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (my/split-window-horizontally)
    (my/split-window-vertically)))

(defun my/swap-window ()
  (interactive)
  (if (eq (selected-frame) (next-frame))
      (error "there is only one frame"))
  (let* ((window1 (selected-window))
         (window2 (next-window nil nil 'visible))
         (buffer1 (current-buffer))
         (buffer2 (window-buffer window2)))
    (if (eq buffer1 buffer2)
        (error "the buffers are the same"))
    (set-window-buffer window1 buffer2)
    (set-window-buffer window2 buffer1)
    (select-window window2)))

(defun my/move-splitter-left-or-up ()
  "Move window splitter left or up"
  (interactive)
  (let ((windmove-wrap-around))
    (cond
     ((windmove-find-other-window 'right)
      (shrink-window-horizontally 2))
     ((windmove-find-other-window 'left)
      (enlarge-window-horizontally 2))
     ((windmove-find-other-window 'up)
      (enlarge-window 2))
     ((windmove-find-other-window 'down)
      (shrink-window 2)))))

(defun my/move-splitter-right-or-down ()
  "Move window splitter right or down."
  (interactive)
  (let ((windmove-wrap-around))
    (cond
     ((windmove-find-other-window 'right)
      (enlarge-window-horizontally 2))
     ((windmove-find-other-window 'left)
      (shrink-window-horizontally 2))
     ((windmove-find-other-window 'up)
      (shrink-window 2))
     ((windmove-find-other-window 'down)
      (enlarge-window 2)))))

;;; ivy
(defun my/swiper-symbol ()
  (interactive)
  (let ((str (and (symbol-at-point) (thing-at-point 'symbol))))
        (counsel-grep-or-swiper str)))

(defun my/swiper-current-kill ()
  (interactive)
  (counsel-grep-or-swiper (current-kill 0)))

(defun my/counsel-rg ()
  (interactive)
  (counsel-rg nil default-directory))

(defun my/counsel-rg-at-point ()
  (interactive)
    (setq current-prefix-arg nil)
    (counsel-rg (and (symbol-at-point) (thing-at-point 'symbol)) default-directory))

(defun my/counsel-rg-current-kill ()
  (interactive)
  (counsel-rg (current-kill 0) default-directory))

(defvar my-main-language-filter "\\.c$\\|\\.h$\\|\\.cpp$\\|\\.hpp$")

(provide 'init-core)
