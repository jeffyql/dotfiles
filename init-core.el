;; -*- lexical-binding: t -*-

;;; better default
(setq debug-on-error t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

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

(delete-selection-mode 1)

;; highlight the current line
(setq-default   auto-revert-verbose nil
                auto-save-default nil
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
(eval-after-load 'tramp '(setenv "SHELL" "/bin/zsh"))
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

;; (use-package counsel :ensure t)

(use-package deadgrep :ensure t)

(use-package expand-region :ensure t)
(use-package ctrlf   :ensure t
  :init
  (ctrlf-mode +1)
  )

(use-package vertico   :ensure t
  :init
  (vertico-mode)
  :bind (:map vertico-map
         ("M-j" . vertico-next)
         ("M-k" . vertico-previous)
         ("M-m" . vertico-buffer))
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  
  ;; (setq completion-in-region-function
  ;;       (lambda (&rest args)
  ;;         (apply (if vertico-mode
  ;;                    #'consult-completion-in-region
  ;;                  #'completion--in-region)
  ;;                args))) ;; `consult-register-store' and the Emacs built-ins.
 
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  
  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  
  :config
  ;; Configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  )

  (use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))
  (use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("M-o" . embark-collect)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
  
;; (use-package ivy
;;   :ensure t
;;   :diminish ivy-mode
;;   :config
;;   (progn
;;     ;(ivy-mode 1)
;;     ;;(setq ivy-use-virtual-buffers t)
;;     (setq ivy-use-selectable-prompt t
;;           )
;;     (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)
;;     (define-key ivy-minibuffer-map (kbd "M-j") 'next-line)
;;     (define-key ivy-minibuffer-map (kbd "M-k") 'previous-line)
;;     ;; keybindings on mac side
;;     (define-key ivy-minibuffer-map (kbd "s-o") 'ivy-occur)
;;     (define-key ivy-minibuffer-map (kbd "s-j") 'next-line)
;;     (define-key ivy-minibuffer-map (kbd "s-k") 'previous-line)
;;     (define-key ivy-minibuffer-map (kbd "M-;") 'kill-ivy-file)
;;     (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
;;     (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
;;     (ivy-mode 1)
;;     )
  
;;  (defun kill-ivy-file (x)
;;    (interactive)
;;    (kill-buffer x)
;;    (ivy--reset-state ivy-last)
;;    )
;;  )

;;; navigation/search

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

(defun my/evil-ex-search-word-forward ()
  (interactive)
  (save-excursion
    (call-interactively 'evil-ex-search-word-forward))
  (beginning-of-thing 'symbol))

(defun my/consult-line-at-point ()
  (interactive)
  (let ((str (and (symbol-at-point) (thing-at-point 'symbol))))
        (consult-line str)))

(defun my/consult-line-current-kill ()
  (interactive)
  (consult-line (current-kill 0)))

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

;;; read

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

;;; write
(defun my/evil-paste-after (&optional arg)
  (interactive "P")
  (if arg
      (counsel-yank-pop)
    (call-interactively 'evil-paste-after)))
 
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

;;; frame tab and window

(defun my/select-tab-or-toggle-buffer (&optional arg)
   (interactive "P")
   (if (integerp arg)
       (tab-bar-select-tab arg)
     (if (equal arg '(16))
         (tab-new)
       (if (equal arg '(4))
           (tab-bar-switch-to-recent-tab)
         (my/toggle-buffer)))))
 
 (defun my/select-window ()
   (interactive)
   (let ((next-window (next-window)))
     (unless (eq next-window (selected-window))
       (select-window next-window)))
   (run-hooks 'window-configuration-change-hook))

(defun my/delete-or-split-window (&optional arg)
  (interactive "P")
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

;;; edit

(defun my/insert-space ()
   (interactive)
   (if (equal major-mode 'vterm-mode)
       (vterm-send-space)
     (insert "\s")))

(defun my/open-line (&optional arg)
  (interactive "P")
  (if arg
      (open-previous-line 1)
    (open-next-line 1)))

;;; misc

(defun my/list-to-indexed-string (group-name)
  (let ((counter 0))
    (mapconcat (lambda (e) (concat e "[" (number-to-string (cl-incf counter)) "]")) group-name " ")))

(defun my/lists-to-string-list-alist (group-names-list)
  (mapcar (lambda (x) (let ((l (symbol-value x))) (cons (mapconcat 'identity l " ") x))) group-names-list))

(defun my/quick-selection-select-group (group-names-list)
  (let ((group-alist
         (my/lists-to-string-list-alist group-names-list))
        group-name)
    (if (= 1 (length group-alist))
        (setq group-name (cdar group-alist))
      (ivy-read "Select group: " (mapcar 'car group-alist)
                :action (lambda (l) (setq group-name
                                          (cdr (assoc l group-alist))))
                :caller 'my/quick-selection-select-group)
      (message "%s" (my/list-to-indexed-string (symbol-value group-name)))
      group-name)))

(defun my/quick-selection-select-item (group-name)
  (let ((counter 0)
        (group (if (symbolp group-name) (symbol-value group-name)))
        len range prompt selected-num)
    (unless group
      (error "group is empty"))
    (unless (listp group)
      (error "not a list"))
    (setq len (length group))
    (if (= 1 len)
     (string-to-char (number-to-string 1))
      (cl-loop for i from 1 to len
               do (setq range (append (list (string-to-char (number-to-string (cl-incf counter)))) range)))
      (setq range (append "?\s" range)
            prompt (my/list-to-indexed-string group)
            prompt (concat "Press number key: " prompt " or press [space] to cancel")
            selected-num (read-char-choice prompt range)))))
;;; ivy

(provide 'init-core)

