(defvar my-command-snippets)
(add-to-list 'savehist-additional-variables 'my-command-snippets)

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
  "e"       'my/vterm-ivy-yank
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
  "uc"      'my/vterm-add-to-collection
  "ud"      'vterm-send-C-d
  "ue"      'vterm-send-C-e
  "ui"      'my/vterm-insert
  "ut"      'compilation-shell-minor-mode
  "uu"      'vterm-undo
  "ux"      'vterm-clear
  "u."      'vterm-send-meta-dot
  "w"       'vterm-send-M-f
  "x"       'vterm-send-C-d
  "y"       'my/vterm-send-key
  "\\"      'my/vterm-send-key
  "<down>"  'vterm-send-down
  "<up>"    'vterm-send-up
  "DEL"     'vterm-send-backspace
  "RET"     'vterm-send-return
  "SPC"     'vterm-send-space
  )

(general-define-key
 :definer 'minor-mode
 :states 'normal
 :keymaps 'vterm-copy-mode
 "a"       'first-error
 "b"       'evil-backward-word-begin
 "h"       'evil-backward-char
 "i"       'vterm-copy-mode-ignore 
 "j"       'evil-next-line
 "k"       'evil-previous-line
 "l"       'evil-forward-char
 "n"       'evil-ex-search-next
 "q"       'my/vterm-copy-mode-done
 "u"       'vterm-copy-mode-ignore 
 "w"       'evil-forward-word-begin
 "x"       'vterm-copy-mode-ignore 
 "y"       'vterm-copy-mode-done
 "DEL"     'my/evil-scroll-up
 "SPC"     'my/evil-scroll-down
 "RET"     'my/vterm-copy-mode-done
  "<escape>"     'my/vterm-copy-mode-done
 )

(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key global-map [xterm-paste] #'my/xterm-paste)
            ))

(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

(defun my/vterm-ivy-yank ()
  (interactive)
  (ivy-read "yank: " my-command-snippets 
            :action #'my/vterm-insert-snippet))

(defun my/vterm-insert-snippet (snippet)
  (vterm-send-string snippet)
  (setq my-command-snippets
        (cons snippet (remove snippet my-command-snippets))))

(defun my/add-or-delete-command-snippet (&optional remove)
  (interactive "P")
  (if remove
      (ivy-read "snippet to remove: " my-command-snippets 
                :action (lambda (s) (setq my-command-snippets (remove s my-command-snippets))))
    (add-to-list 'my-command-snippets (read-string "add new term: "))))

(defun my/vterm-send-key ()
  (interactive)
  (let ((base (event-basic-type last-input-event)))
    (vterm-send-key (char-to-string base))))

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
             ((eq base 'return) (message "done") (throw :exit nil))
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
             ((= base ?m) (message "done") (throw :exit nil))
             (t nil)))
           (t nil)))
        (message "======insert=======insert======insert======")
          ))))

(defun my/vterm-send-return ()
  (interactive)
  (vterm-send-return)
  (evil-normal-state))

(defun my/vterm-copy-mode-done ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end)))
  (vterm-copy-mode -1))

(defun my/xterm-paste (event)
  "Handle the start of a terminal paste operation."
  (interactive "e")
  (unless (eq (car-safe event) 'xterm-paste)
    (error "xterm-paste must be found to xterm-paste event"))
  (let* ((inhibit-read-only t)
         (pasted-text (nth 1 event))
         (interprogram-paste-function (lambda () pasted-text)))
    (if (eq major-mode 'vterm-mode)
        (progn
          (vterm-copy-mode-done)
          (vterm-yank))
      (yank))))

(defvar vterm-command-beginging 1)

(defvar my-vterm-selected-buffer nil)
;; (add-to-list 'vterm-keymap-exceptions "C-l")
(add-to-list 'vterm-keymap-exceptions "M-:")
;; (add-to-list 'vterm-keymap-exceptions "<escape>")
;; (add-to-list 'vterm-keymap-exceptions (kbd "ESC"))

(global-set-key (kbd "M-:") 'eval-expression)

(add-hook 'vterm-mode-hook 'my-init-esc)


(defun my/vterm-exit-function (buf)
  (evil-normal-state)
  )

(add-to-list 'vterm-exit-functions 'my/vterm-exit-function)

(defun my/vterm-insert ()
  (interactive)
  (evil-emacs-state)
  (vterm-reset-cursor-point))

(defun my/vterm-insert-line ()
  (interactive)
  (evil-emacs-state)
  (vterm-send-C-a))

(defun my/vterm-append ()
  (interactive)
  (evil-emacs-state)
  (vterm-send-C-f))

(defun my/vterm-append-line ()
  (interactive)
  (evil-emacs-state)
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
  (evil-emacs-state)
  (vterm-send-C-r))

(defun my/vterm-capture ()
  (interactive)
  (let* ((buf (current-buffer))
         (beg vterm-command-beginging)
         (end (point-max))
         (bn "vterm capture")
         (b (get-buffer bn)))
    (if (bufferp b)
        (kill-buffer b))
    (vterm-copy-mode 1)
    (setq b (get-buffer-create bn))
    (with-current-buffer b
      (insert-buffer-substring buf beg end))
    (vterm-copy-mode -1)
    (switch-to-buffer b)
    ))

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

(defun get-create-vterm (buf-name &optional other-window)
  (let ((default-directory "~")
        (buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (generate-new-buffer buf-name))
      (with-current-buffer buffer
        (vterm-mode)))
    (if (or (one-window-p) (not (eq major-mode 'org-mode)))
        (pop-to-buffer-same-window buffer)
      (switch-to-buffer-other-window buffer) 
      (other-window 1)
      )))

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


(provide 'init-vterm)
