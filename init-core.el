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

(defun my/get-recentf-project (filter &optional arg)
  (let (project-root root)
    (unless arg
      (setq project-root 
            (catch 'done
              (cl-loop for f in recentf-list do
                       (when (string-match-p filter f)
                         (setq root (projectile-project-root (file-name-directory f)))
                         (if root (throw 'done root)))))))
    (unless project-root
      (setq project-root 
            (projectile-project-root (projectile-completing-read
                                      "Switch to project: " projectile-known-projects))))
    project-root))

(defun my/get-not-displayed-from-file-list (list)
  (let* ((next-buf (unless (one-window-p) (window-buffer (next-window))))
         (next-file (if next-buf (buffer-file-name next-buf))))
    (if buffer-file-name (setq list (delete buffer-file-name list)))
    (if next-file (setq list (delete next-file list)))
    list))
  
(defun my/recentf-main-language-by-project (&optional arg)
  (interactive "P")
  (let* ((project-root (my/get-recentf-project my-main-language-filter arg))
         (list (cl-remove-if-not (lambda (f) (and (string-match my-main-language-filter f)
                                                  (string-prefix-p project-root f)))
                                 recentf-list))
         (list (my/get-not-displayed-from-file-list list)) 
         project-root-name prompt)
    (setq list (mapcar (lambda (f) (file-relative-name f project-root)) list)
          project-root-name (file-name-nondirectory (directory-file-name project-root))
          prompt (concat "[" project-root-name "]: "))
    (ivy-read  prompt list
               :action (lambda (f)
                         (with-ivy-window
                           (find-file (expand-file-name f project-root))))
               :caller 'counsel-recentf)))

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

(defun my/last-file-by-type-and-project (filter project)
  (let (file buf)
    (catch 'done
      (cl-loop for file in recentf-list do
               (setq buf (get-file-buffer file))
               (if (and (string-match-p filter file)
                        (or (not buf) (not (get-buffer-window buf)))
                        (string-prefix-p project file))
                   (throw 'done file))))))

(defun my/open-last-file-by-type-and-project (filter project)
  (let ((file (my/last-file-by-type-and-project filter project)))
    (when (and file (file-exists-p file))
      (find-file file))))

(defun my/last-main-language-file (&optional arg)
  (interactive "P")
  (let ((project (my/get-recentf-project my-main-language-filter arg)))
    (my/open-last-file-by-type-and-project
     my-main-language-filter
     project)))

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

(defun my/send-command ()
  (interactive)
  (let ((buf (unless (one-window-p) (window-buffer (next-window))))
        mode beg end cmd-str window)
    (if (one-window-p)
        (error "only one window"))
    (with-current-buffer buf
      (unless (or (eq major-mode 'vterm-mode)
                  (eq major-mode 'sql-interactive-mode))
        (error "next buffer not a shell buffer"))
      (setq mode major-mode))
    (cond
     ((use-region-p)
      (setq beg (region-beginning) 
            end (region-end))
      (deactivate-mark t))
     ((and (eq major-mode 'org-mode) (org-in-item-p))
      (save-excursion
        (beginning-of-line)
        (looking-at org-list-full-item-re)
        (setq pos (car (last (match-data))))
        (goto-char pos)
        (if (looking-at "[ \t]*\\(\\$\\|#\\|>\\)[ \t]+")
            (goto-char (match-end 0)))
        (setq beg (point)
              end (line-end-position))))
     ((and (eq major-mode 'org-mode) (org-in-src-block-p))
      (setq cmd-str (org-remove-indentation (org-element-property :value (org-element-context)))))
     (t
      (save-excursion
        (setq beg (line-beginning-position)
              end (line-end-position)))))
    (unless cmd-str
      (setq cmd-str (buffer-substring-no-properties beg end)))
    (cond
     ((eq mode 'sql-interactive-mode)
      (let ((sql-buffer buf))
        (sql-send-string cmd-str)))
     ((eq mode 'vterm-mode)
      (with-current-buffer buf
        (vterm-send-string cmd-str)
        (vterm-send-return)))
     (t)
     )))

(defun my/switch-indirect-narrow (&optional arg)
  (interactive "P")
  (if (buffer-base-buffer)
      (let ((start (window-start))
            (pos (point))
            (buf (current-buffer)))
        (pop-to-buffer-same-window (buffer-base-buffer))
        (set-window-start (selected-window) start)
        (goto-char pos) ;; anchor cursor
        (unless (= (window-start) (point-min))
          (scroll-down 1))
        (goto-char pos))
    (let* ((pos (point))
           (win-start (window-start))
           (win-end (window-end))
           (region (if (use-region-p) t))
           (file-name (buffer-name (buffer-base-buffer)))
           (prefix
            (when (and (not (equal arg '(4))) (not region))
              (cond
               ((eq major-mode 'c++-mode)
                (car (c-defun-name-and-limits nil)))
               ((derived-mode-p 'prog-mode)
                (which-function))
               ((eq major-mode 'org-mode)
                (org-get-heading t t))
               (t nil))))
           buf-name beg end)
      (unless prefix
        (setq prefix (read-string (concat "buffer name prefix: "))))
      (setq buf-name (concat ">>" prefix "<< [file:" file-name "]"))
      (if (get-buffer buf-name)
          (progn
            (pop-to-buffer-same-window buf-name)
            (if (and (>= win-start (point-min))
                     (<= win-end (point-max)))
                (set-window-start (selected-window) win-start))
            (goto-char pos)
            )
        (when region
          (setq  beg (region-beginning)
                 end (region-end))
          (deactivate-mark t)
          )
        ;; (remove-overlays beg end)
        (clone-indirect-buffer buf-name nil)
        (pop-to-buffer-same-window buf-name)
        (cond
         (region
          (narrow-to-region beg end))
         ((derived-mode-p 'prog-mode)
          (narrow-to-defun))
         ((eq major-mode 'org-mode)
          (org-narrow-to-subtree)))
        (goto-char pos)
        (recenter nil)))))

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
