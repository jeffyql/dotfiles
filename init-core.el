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

;; enable reuse directory buffer
(put 'dired-find-alternate-file 'disabled nil)

;;; config packages
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package avy
  :ensure t
  :config
  (setq avy-background nil
        avy-all-windows (quote all-frames))
  )

(use-package counsel :ensure t)

(use-package deadgrep
  :ensure t
  )
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
    (use-package evil-commentary :ensure t)
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

(use-package magit :ensure t)

(use-package org
  :ensure t
  )

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

(use-package whitespace :ensure t)

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

(defun my/scroll-down (&optional arg)
  (interactive "P")
  (if arg
      (evil-scroll-down 0)
    (evil-scroll-page-down 1)
  ))

(defun my/scroll-up (&optional arg)
  (interactive "P")
  (if arg
      (evil-scroll-up 0)
    (evil-scroll-page-0p 1)
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

(defun my/swiper (&optional arg)
  (interactive "P")
  (let ((str (if (equal arg '(4))
                 (and (symbol-at-point) (thing-at-point 'symbol)))))
    (swiper str)))

(defun my/swiper-current-kill ()
  (interactive)
  (swiper (current-kill 0)))

(defun my/counsel-rg-at-point (&optional arg)
  (interactive "P")
    (setq current-prefix-arg nil)
    (counsel-rg (and (equal arg '(4)) (symbol-at-point) (thing-at-point 'symbol))))

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
;;; frame and window
(defun my/next-frame ()
  (interactive)
  (let ((next-frame
         (if (<= (length (frame-list)) 2)
             (make-frame)
           (next-frame))))
    (select-frame-set-input-focus next-frame)
    (run-hooks 'window-configuration-change-hook)))
 
(defun my/select-window (&optional arg)
  (interactive "P")
  (if (integerp arg)
      (winum-select-window-by-number arg)
    (if arg
        (my/next-frame)
      (other-window 1 'visible)
      (select-frame-set-input-focus (selected-frame)))))
 
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

;;; file
(defvar my/counsel-recentf-file-extension nil)

(defun my/counsel-recentf-set-file-extension ()
  (interactive)
  (let ((extension (concat "\\." (read-string "file extension string:") "$")))
    (customize-save-variable 'my/counsel-recentf-file-extension extension)
  ))
(defvar my-main-language-filter "\\.c$\\|\\.h$\\|\\.cpp$\\|\\.hpp$")

(defun my/recentf-by-type (filter &optional include)
  (let* ((list (if include
                    (cl-remove-if-not  (lambda (x) (string-match filter x)) recentf-list)
                  (cl-remove-if  (lambda (x) (string-match filter x)) list)))
         (list (if (equal (buffer-file-name) (car list))
                   (cdr list) list)))
    (ivy-read "Select file: " list
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :caller 'counsel-recentf)))

(defun my/recentf-by-type-and-project (filter &optional include arg)
  (interactive "P")
  (let* ((project-root
          (or (and (not arg)
                   (projectile-project-root default-directory))
              (projectile-completing-read
               "Switch to project: " projectile-known-projects)))
         (default-directory project-root)
         (list (projectile-recentf-files))
         (list (if include
                   (cl-remove-if-not  (lambda (x) (string-match filter x)) list)
                 (cl-remove-if  (lambda (x) (string-match filter x)) list)))
         (list (if (and buffer-file-name
                        (equal (car list)
                               (file-relative-name buffer-file-name project-root)))
                   (cdr list) list))
         (next-buf (window-buffer (next-window (selected-window) nil 'visible)))
         (next-buf-file (buffer-file-name next-buf))
         (list (if next-buf-file
                   (delete (file-relative-name next-buf-file project-root) list)
                 list))
         (project-root-name (file-name-nondirectory (directory-file-name project-root)))
         (prompt (concat "[" project-root-name "]: ")))
    (ivy-read  prompt list
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :caller 'counsel-recentf)))

(defun my/recentf-main-language (&optional arg)
   (interactive "P")
   (my/recentf-by-type-and-project my-main-language-filter t arg))
 
 (defun my/recentf-el ()
   (interactive)
   (my/recentf-by-type "\\.el$\\|\\.el.gz$" t))
 
 (defun my/recentf-org ()
   (interactive)
   (my/recentf-by-type "\\.org$" t))
 
 (defun my/recentf-misc (&optional arg)
   (interactive "P")
   (my/recentf-by-type (concat my-main-language-filter "\\|\\.org$\\|\\.el$\\|\\.el\\.gz") nil arg))
 
(defun my/last-file-by-type (filter)
  (let* ((next-window-file
          (and (not (eq (selected-frame) (next-frame)))
               (buffer-file-name
                (window-buffer (next-window (selected-window) nil 'visible)))))
         file)
    (setq file
          (catch 'done
            (cl-loop for file in recentf-list do
                     (unless (or (not (string-match filter file))
                                 (equal file buffer-file-name)
                                 (equal file next-window-file))
                       (throw 'done file)))))
    (if (file-exists-p file)
        (find-file file))))

(defun my/last-main-language-file ()
  (interactive)
  (my/last-file-by-type my-main-language-filter))

(defun my/last-org-file ()
  (interactive)
  (my/last-file-by-type "\\.org$"))

 (defun my/projectile-select-project ()
   (interactive)
   (let* ((dir (read-directory-name "Select default project: " "~/" nil t))
          (project-root (projectile-project-root dir)))
     (unless project-root
       (error "not a project"))
     (setq my-default-projectile-project project-root)
     (let ((default-directory my-default-projectile-project))
       (projectile-dired))))
 
 (defun my/projectile-find-dir (&optional arg)
   (interactive "P")
   (let ((default-directory (if arg (projectile-completing-read
                                     "Switch to project: " projectile-known-projects)
                               default-directory)))
     (projectile-find-dir)))
 
 (defun my/projectile-find-file (&optional arg)
   (interactive "P")
   (let ((default-directory (if arg (projectile-completing-read
                                     "Switch to project: " projectile-known-projects)
                               default-directory)))
     (projectile-find-file)))
 
 (defun my/projectile-dired ()
   (interactive)
   (let ((projectile-switch-project-action 'projectile-dired))
     (projectile-switch-project)))

(setq my/counsel-rg-base-command-prefix
      "rg -i -M 512 --no-heading --line-number --color never --follow ")

(defun my/ffap ()
  (interactive)
  (let (msg loc filename dir line))
    (if (or (symbol-value 'compilation-minor-mode)
            (eq major-mode 'compilation-mode))
        (setq msg (get-text-property (point) 'compilation-message)
              loc (and msg (compilation--message->loc msg))
              filename (caar (compilation--loc->file-struct loc))
              filename (file-name-nondirectory filename)
              line (cadr loc))
      (setq filename (ffap-string-at-point))
      (save-excursion
        (beginning-of-line 2)
        (if (looking-at "\\([0-9]+\\):")
          (setq line (string-to-number (match-string 1))))))
    (setq dir (ivy-dired-history--read-file-name "directory: ")
          filename (concat dir filename))
    (if (not (file-exists-p filename))
        (message "%s doesn't exist" filename)
      (ivy-dired-history--update dir)
      (find-file filename)
      (if line
          (goto-line line))))

(setq my-default-projectile-project nil)

(defun my/projectile-select-project ()
  (interactive)
  (let* ((dir (read-directory-name "Select default project: " "~/" nil t))
         (project-root (projectile-project-root dir)))
    (unless project-root
      (error "not a project"))
    (setq my-default-projectile-project project-root)))

(defun my/projectile-find-dir (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (let ((projectile-switch-project-action 'projectile-find-dir))
        (projectile-switch-project))
      (projectile-find-dir)))

(defun my/projectile-find-file (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (let ((projectile-switch-project-action 'projectile-find-file))
        (projectile-switch-project))
      (projectile-find-file)))

(defun my/projectile-recentf (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (let ((projectile-switch-project-action 'projectile-recentf))
        (projectile-switch-project))
      (projectile-recentf)))

(defun my/counsel-narrowed-indirect (&optional arg)
   (interactive "P")
   (let* ((buffer-names (mapcar #'buffer-name (buffer-list)))
          (narrowed-indirect-list
           (cl-remove-if-not
            (lambda (b) (and (> (length b) 4) (equal ">>"(substring b 0 2))))
            buffer-names)))
     (if arg
       (unless (symbol-at-point)
         (error "symbol not found at point")))

     (ivy-read "Select indirect buffer: " narrowed-indirect-list
               :action (lambda (b)
                         (with-ivy-window
                           (switch-to-buffer b)))
               :initial-input (if arg (thing-at-point 'symbol)))))

 (defun my/goto-indirect-narrow-at-point ()
   (interactive)
   (let ((buffer-names (mapcar (lambda (b) (buffer-name b)) (buffer-list)))
         (func-name (thing-at-point 'symbol))
         buf-name)
     (unless func-name
       (error "no symbol found at point"))
     (setq func-name (concat func-name "<<"))
     (setq buf-name (seq-find (lambda (s) (and (> (length s) 4)
                                               (equal ">>" (substring s 0 2))
                                               (string-match-p (regexp-quote func-name) s)))
                              buffer-names))
     (unless (stringp buf-name)
       (error "buffer not found"))
     (switch-to-buffer buf-name)))

(provide 'init-core)
