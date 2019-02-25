;;; -*- lexical-binding: t -*-

(setq debug-on-error t)

(add-hook 'after-init-hook
          (lambda () (setq debug-on-error nil)))

(setq bookmark-default-file "~/db/bookmarks")

(require 'dired-x)
(require 'ediff)
(require 'paren)
(require 'which-func)
(require 'winner)

(when (eq system-type 'darwin)
      (set-default-font "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))


(setq custom-file "~/.emacs-custom.el")

(electric-pair-mode 1)

(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

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

(winner-mode 1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ace-window
  :ensure t
  :config
  (setq aw-scope 'frame
        aw-swap-invert t))

(use-package avy
  :ensure t
  :config
  (setq avy-background nil)
  (setq avy-keys (nconc (number-sequence ?a ?z)))
  )

;(use-package bind-map :ensure t)

(use-package counsel :ensure t)

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
    (use-package evil-ediff :ensure t)
    (use-package evil-matchit :ensure t)
    )

  (use-package undo-tree
    :ensure t
    :diminish undo-tree-mode)
)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )


(use-package expand-region :ensure t)

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
  (use-package ivy-hydra :ensure t)
  )

(use-package ivy-dired-history
  :ensure t
  :init
  (progn
    (savehist-mode 1)
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)
    )
  )

(use-package org
  :ensure t
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  )

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

(defun my/evil-end-of-line ()
  (interactive)
  (evil-end-of-line))

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

(setq large-file-size (* 1024 1024))

(defun my/quit-this-buffer (&optional arg)
  (interactive "P")
  (let ((cur-buf (current-buffer)))
    (cond
     ((equal (buffer-name) "*scratch*")
      (quit-window))
     ((equal (buffer-name) "*Messages*")
      (quit-window))
     ;; dired or help buffers
     ((and (not buffer-file-name) (not (buffer-base-buffer)))
      (kill-this-buffer))
     ((buffer-modified-p) ;; buffer is not saved
      (message "buffer modified"))
     ((cl-loop for buf in (buffer-list)
               if (eq cur-buf (buffer-base-buffer buf))
               return t)  ;; buffer has clones
      (if (yes-or-no-p "derived buffer(s) exist, kill it anyway?")
          (kill-this-buffer)))
     (t  ;; regular or narrowed buffers
        (kill-this-buffer)))))

(defun my/save-buffer (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (save-some-buffers)
      (call-interactively 'save-buffer)))

(defun my/swiper (&optional arg)
  (interactive "P")
  (let ((str (if (equal arg '(4))
                 (and (symbol-at-point) (thing-at-point 'symbol)))))
    (swiper str)))


(defun my/toggle-buffer ()
  "Toggle buffers, ignoring certain ones."
  (interactive)
  (catch 'done
    (dolist (buf (buffer-list))
      (unless (or (equal (current-buffer) buf)
                  (and (not (one-window-p))
                       (eq (window-buffer (next-window)) buf))
                  (with-current-buffer buf
                    (string-match "^ +\\*" (buffer-name))
                    ))
        (switch-to-buffer buf)
        (throw 'done t)))
    (message "no more candidate buffer")))

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

;; todo, remove ace dependenc
(defun my/switch-window (&optional arg)
  (interactive "P")
  (if (one-window-p)
      (if (equal arg '(4))
         (my/split-window-vertically)
        (my/split-window-horizontally))
    (other-window 1)))

(defun move-splitter-left ()
  "Move window splitter left."
  (interactive)
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally 2)
    (enlarge-window-horizontally 2)))

(defun move-splitter-right ()
  "Move window splitter right."
  (interactive)
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally 2)
    (shrink-window-horizontally 2)))

(defun move-splitter-up ()
  "Move window splitter up."
  (interactive)
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window 2)
    (shrink-window 2)))

(defun move-splitter-down ()
  "Move window splitter down."
  (interactive)
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window 2)
    (enlarge-window 2)))

(defvar my/counsel-recentf-file-extension nil)

(defun my/counsel-recentf-set-file-extension ()
  (interactive)
  (let ((extension (concat "\\." (read-string "file extension string:") "$")))
    (customize-save-variable 'my/counsel-recentf-file-extension extension)
  ))

(defun my/counsel-recentf-misc ()
  (interactive)
  (let ((filter "\\.c$\\|\\.h$\\|\\.cpp$\\|\\.hpp$\\|\\.el$\\|\\.el.gz$\\|\\.json$\\|\\.log$\\|\\.org$\\|\\.py$\\|\\.sh$\\|\\.txt$\\|\\.xml$\\|Dockerfile\\|\\.[cs]ql$")
        (file-list (mapcar #'substring-no-properties recentf-list)))
    (ivy-read "Select file: " (cl-remove-if  (lambda (x) (string-match filter x)) file-list)
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :caller 'counsel-recentf)))

(defun my/counsel-recentf-filtered (filter &optional project-root)
  (let (file-list)
    (setq file-list
          (if project-root
              (cl-remove-if-not (lambda (f)
                                  (and (string-prefix-p project-root f)
                                       (string-match filter f)))
                                recentf-list)
            (cl-remove-if-not  (lambda (x) (string-match filter x)) recentf-list))
          file-list (if buffer-file-name
                        (delete buffer-file-name file-list)
                      file-list))
    (if project-root
        (setq file-list (mapcar (lambda (f) (file-relative-name f project-root)) file-list)))
    (ivy-read (concat "[" project-root "] Select file: ") file-list
              :action (lambda (f)
                        (with-ivy-window
                          (find-file (if project-root (expand-file-name f project-root)
                                       f))))
              :caller 'counsel-recentf)))

(defun my/counsel-recentf-designated ()
  (interactive)
  (if (stringp my/counsel-recentf-file-extension)
      (my/counsel-recentf-filtered my/counsel-recentf-file-extension)))

(setq my-c++-project-root nil)

(defun my/counsel-recentf-c (&optional projectile)
  (interactive "P")
  (if projectile
      (setq my-c++-project-root (projectile-expand-root
                             (projectile-completing-read "Switch to open project: "
                                                         projectile-known-projects))))
  (my/counsel-recentf-filtered "\\.c$\\|\\.h$\\|\\.cpp$\\|\\.hpp$" my-c++-project-root))

(defun my/counsel-recentf-dockerfile (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "dockerfile" projectile))

(defun my/counsel-recentf-cmake (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "CMakeLists\\.txt$" projectile))

(defun my/counsel-recentf-el (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.el$\\|\\.el.gz$" projectile))

(defun my/counsel-recentf-json (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.json$" projectile))

(defun my/counsel-recentf-log (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.log$" projectile))

(defun my/counsel-recentf-org (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.org$" projectile))

(defun my/counsel-recentf-py (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.py$" projectile))

(defun my/counsel-recentf-sql-cql (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.[cs]ql$" projectile))

(defun my/counsel-recentf-sh (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.sh$" projectile))

(defun my/counsel-recentf-txt (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.txt$" projectile))

(defun my/counsel-recentf-xml (&optional projectile)
  (interactive "P")
  (my/counsel-recentf-filtered "\\.xml$" projectile))

(setq my/counsel-rg-base-command-prefix
      "rg -i -M 512 --no-heading --line-number --color never --follow ")

(defun my/counsel-rg-at-point (&optional arg)
  (interactive "P")
    (setq current-prefix-arg nil)
    (counsel-rg (and (equal arg '(4)) (symbol-at-point) (thing-at-point 'symbol))))

(defun my/ffap ()
  (interactive)
  (let* ((file (ffap-string-at-point))
         (dir (ivy-dired-history--read-file-name "directory: "))
         (file-name (concat dir file)))
    (when (file-exists-p file-name)
      (ivy-dired-history--update dir)
      (find-file file-name))))

(provide 'init-core)
