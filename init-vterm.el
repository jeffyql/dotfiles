(setq my-command-snippets-file (concat my-mib-dir "command_snippets.txt")
      my-command-snippets-max-size 2000)

(if (file-exists-p my-command-snippets-file)
    (load my-command-snippets-file)
  (when (y-or-n-p "initialize command snippets? ")
    (setq my-command-snippets nil)))

(use-package vterm
  :ensure t
  :load-path "/Users/jeff/emacs-libvterm"
  :config
  (progn
    (setq vterm-max-scrollback 100000
          vterm-clear-scrollback t)
    ))

(general-def '(normal motion visual) 'vterm-mode-map
  "a"       'my/vterm-append
  "A"       'my/vterm-append-line
  "b"       'vterm-send-M-b
  "c"       'vterm-send-C-c
  "d"       nil
  "db"      'vterm-send-meta-backspace
  "dg"      nil
  "dga"     'vterm-send-C-u
  "dge"     'vterm-send-C-k
  "dw"      'vterm-send-M-d
  "e"       'my/vterm-yank-command-snippet
  "h"       'vterm-send-C-b
  "i"       'my/vterm-insert
  "I"       'my/vterm-insert-line
  "j"       'vterm-send-down
  "k"       'vterm-send-up
  "l"       'vterm-send-C-f
  "n"       'my/vterm-send-key
  "o"       (lambda () (interactive) (vterm-copy-mode 1))
  "p"       'vterm-yank
  "q"       'my/vterm-send-key
  "r"       'my/vterm-send-C-r
  "u"       nil
  "ua"      'vterm-send-C-a
  "uc"      'my/vterm-save-current-command
  "ud"      'vterm-send-C-d
  "ue"      'vterm-send-C-e
  "ui"      'my/vterm-insert
  "ut"      'compilation-shell-minor-mode
  "uu"      'vterm-undo
  "ux"      'vterm-clear
  "u."      'vterm-send-meta-dot
  "w"       'vterm-send-M-f
  "x"       'vterm-send-C-d
  "\\"      'my/vterm-send-slash-key
  "<down>"  'vterm-send-down
  "<up>"    'vterm-send-up
  "DEL"     'my/vterm-scroll-up
  "<backspace>"  'my/vterm-scroll-up
  "RET"     'vterm-send-return
  "<return>" 'vterm-send-return
  "SPC"     'vterm-send-space
  "<xterm-paste>"  'my/xterm-paste
  )

(general-def '(emacs insert) 'vterm-mode-map
  "<xterm-paste>" 'my/xterm-paste
  )

(general-define-key
 :definer 'minor-mode
 :states '(normal motion)
 :keymaps 'vterm-copy-mode
 "a"       'first-error
 "b"       'evil-backward-word-begin
 "h"       'evil-backward-char
 "i"       'my/vterm-insert
 "j"       'evil-next-line
 "k"       'evil-previous-line
 "l"       'evil-forward-char
 "n"       'evil-ex-search-next
 "q"       'vterm-copy-mode-done
 "u"       'vterm-copy-mode-ignore 
 "v"       'evil-visual-char
 "w"       'evil-forward-word-begin
 "x"       'vterm-copy-mode-ignore
 "DEL"     'my/evil-scroll-up
 "SPC"     'my/evil-scroll-down
 "RET"     'vterm-copy-mode-done
 "<escape>"    'my/vterm-cancel-copy-mode
 )

(general-define-key
 :definer 'minor-mode
 :states 'visual
 :keymaps 'vterm-copy-mode
 "a"  'mark-whole-buffer
 "i"  'evil-visual-line
 "o"  'evil-visual-block
 "u"  'er/contract-region
 "v"  'er/expand-region
 "RET"     'vterm-copy-mode-done
 "<escape>"    'my/vterm-cancel-copy-mode
 )

(defun my/xterm-paste (event)
  "Handle the start of a terminal paste operation."
  (interactive "e")
  (unless (eq (car-safe event) 'xterm-paste)
    (error "xterm-paste must be found to xterm-paste event"))
  (let* ((inhibit-read-only t)
         (pasted-text (nth 1 event))
         (interprogram-paste-function (lambda () pasted-text)))
    (vterm-yank)))

(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function #'(lambda(_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
               #'(lambda(str) (vterm-send-string str t))))
            (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

(add-to-list 'my-saved-lists-alist (cons 'my-command-snippets my-command-snippets-file))

(defun my/vterm-yank-command-snippet ()
  (interactive)
  (let ((selected (my/saved-lists-select 'my-command-snippets)))
    (if (eq major-mode 'vterm-mode)
        (vterm-send-string selected)
      (insert selected))))

(defun my/add-or-remove-command-snippet (&optional remove)
  (interactive "P")
  (my/saved-lists-add-or-remove-element 'my-command-snippets remove))

(defun my/vterm-send-key ()
  (interactive)
  (let ((base (event-basic-type last-input-event)))
    (vterm-send-key (char-to-string base))))

(defun my/vterm-scroll-up ()
  (interactive)
  (vterm-copy-mode 1)
  (evil-scroll-up 0))

(defun my/vterm-send-slash-key ()
  (interactive)
  (let ((inhibit-read-only t)
        event modifier base)
    (vterm-send-key "\\")
    (setq event (read-event)
          modifier (event-modifiers event)
          base (event-basic-type event))
    (if modifier
        (error "invalid input"))
    (unless (characterp base)
      (error "invalid input"))
    (vterm-send-key (char-to-string base))
    (vterm-send-return)))

(defun my/vterm-insert-state (&optional tab-func)
  (interactive)
  (unless vterm--term
    (error "Not in a vterm"))
  (let ((inhibit-read-only t)
        event modifier base shift meta ctrl pos)
    (message "======insert=======insert======insert======")
    (catch :exit
      (while t
        (setq event (read-event)
              modifier (event-modifiers event)
              base (event-basic-type event))
        (if (not modifier)
            (cond
             ((eq base 'return) (message "done") (evil-emacs-state) (throw :exit nil))
             ((eq base 'backspace) (vterm-send-backspace))
             ((eq base 'tab)
              (if tab-func (funcall tab-func) (vterm-send-tab)))
             ((eq base 'escape) (vterm-send-C-f) (sit-for 0.1) (throw :exit nil))
             (t
              (if (characterp base)
                  (vterm-send-key (char-to-string base))
                (message "unsupported input")
                )))
          (cond
           ((memq 'shift modifier)
            (if (characterp base)
                (vterm-send-key (char-to-string (upcase base)))))
           ((memq 'control modifier)
            (cond
             ((= base ?\[) (vterm-send-C-f) (sit-for 0.1) (throw :exit nil))
             ((= base ?i)
              (if tab-func (funcall tab-func) (vterm-send-tab)))
             ((= base ?m) (message "done") (evil-emacs-state) (throw :exit nil))
             (t nil)))
           (t nil)))
        (message "======insert=======insert======insert======")
          ))))


(defun my/vterm-cancel-copy-mode ()
  (interactive)
  (if (region-active-p)
      (deactivate-mark t))
  (if vterm-copy-mode
      (vterm-copy-mode -1)
    (vterm-reset-cursor-point)))

(defvar my-vterm-selected-buffer nil)
;; (add-to-list 'vterm-keymap-exceptions "C-l")
(add-to-list 'vterm-keymap-exceptions "M-:")
;; (add-to-list 'vterm-keymap-exceptions "<escape>")
;; (add-to-list 'vterm-keymap-exceptions (kbd "ESC"))

(global-set-key (kbd "M-:") 'eval-expression)

(defun my/vterm-init-hook ()
  (my-init-esc)
  (setq-local evil-move-cursor-back nil)
  )

(add-hook 'vterm-mode-hook 'my/vterm-init-hook)

(defun my/vterm-insert ()
  (interactive)
  (my/vterm-cancel-copy-mode)
  (evil-insert-state))

(defun my/vterm-insert-line ()
  (interactive)
  (my/vterm-insert)
  (vterm-send-C-a))

(defun my/vterm-append ()
  (interactive)
  (my/vterm-insert)
  (vterm-send-C-f))

(defun my/vterm-append-line ()
  (interactive)
  (my/vterm-insert)
  (vterm-send-C-e))

(defun my/vterm-kill-whole-line ()
  (interactive)
  (vterm-send-key "a" nil nil t)
  (vterm-send-key "k" nil nil t)
  )

(defun my/vterm-send-n ()
  (interactive)
  (vterm-send-key "n"))

(defun my/vterm-send-q ()
  (interactive)
  (vterm-send-key "q")
  (vterm-send-return))

(defun my/vterm-send-y ()
  (interactive)
  (vterm-send-key "y")
  (vterm-send-return))

(defun my/vterm-send-C-r ()
  (interactive)
  (vterm-send-C-r)
  (my/vterm-insert-state 'vterm-send-C-r))

(defun my/vterm-select-buffer ()
  (interactive)
  (let* ((buf-num (read-number "Terminal Number: "))
         (buf-name (concat "*vterminal<" (number-to-string buf-num) ">*"))
         (buf (get-buffer buf-name)))
    (unless (bufferp buf)
      (error "terminal %s doesn't exist" buf-name))
    (setq my-vterm-selected-buffer buf-name)
    (message "%s is choosen" my-vterm-selected-buffer)
  ))

(defun get-create-vterm (buf-name)
  (let ((default-directory "~")
        (pop-up-windows nil)
        (buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buf-name))
      (with-current-buffer buffer
        (vterm-mode)))
    (if (or (one-window-p) (not (eq major-mode 'org-mode)))
        (pop-to-buffer-same-window buffer)
      (pop-to-buffer buffer t) 
      )))

(defun vterm-by-number ()
  (interactive)
  (let ((base (event-basic-type last-input-event)))
    (unless (and (characterp base) (>= base ?0) (<= base ?9))
      (error "not a digit"))
    (get-create-vterm (concat "vterm-" (char-to-string base)))))

(defvar known-vterms nil)

(defun goto-vterm ()
  (interactive)
  (let (vec host dir vterm)
    (if (not (file-remote-p default-directory))
        (setq vterm "vterm"
              dir default-directory)
      (setq vec (tramp-dissect-file-name default-directory)
            host (nth 4 vec)
            vterm (car (split-string host "\\."))
            dir (nth 6 vec)))
    (get-create-vterm vterm)
    (vterm-send-string (concat "cd" " " dir))
    (vterm-send-return)))

(defun my/vterm-save-current-command ()
  (interactive)
  (let (beg end cmd)
    (vterm-reset-cursor-point)
    (vterm-send-C-a)
    (sleep-for 0.1)
    (setq beg (vterm--get-cursor-point))
    (vterm-send-C-e)
    (sleep-for 0.1)
    (setq end (vterm--get-cursor-point))
    (setq cmd (buffer-substring-no-properties beg end))
    (message "saved command: %s" cmd)
    (kill-new cmd)))

(provide 'init-vterm)
