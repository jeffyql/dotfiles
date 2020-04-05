(defvar my-vterm-yank-collection nil "vterm yank collection")
(add-to-list 'savehist-additional-variables 'my-vterm-yank-collection)

(use-package vterm
  :ensure t
  :load-path "/Users/jeff/emacs-libvterm"
  :config
  (progn
    (setq vterm-max-scrollback 100000)
    ))
(general-def '(normal motion visual) 'vterm-mode-map
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
  "j"       'vterm-send-down
  "k"       'vterm-send-up
  "l"       'vterm-send-C-f
  "o"       'vterm-copy-mode
  "p"       'vterm-yank
  "r"       'my/vterm-send-C-r
  "u"       nil
  "ua"      'vterm-send-C-a
  "uc"      'my/vterm-add-to-collection
  "ue"      'vterm-send-C-e
  "ui"      'my/vterm-insert
  "ul"      'vterm-clear
  "un"      'my/vterm-send-n
  "uq"      'my/vterm-send-q
  "uu"      'vterm-undo
  "uy"      'my/vterm-send-y
  "u."      'vterm-send-meta-dot
  "w"       'vterm-send-M-f
  "<down>"  'vterm-send-down
  "<up>"    'vterm-send-up
  "DEL"     'vterm-send-backspace
  "RET"     'vterm-send-return
  "SPC"     'vterm-send-space
  )
(general-define-key
 :definer 'minor-mode
 :states '(normal motion visual)
 :keymaps 'vterm-copy-mode
 "b"       'evil-backward-word-begin
 "h"       'evil-backward-char
 "j"       'evil-next-line
 "k"       'evil-previous-line
 "l"       'evil-forward-char
 "o"       'vterm-copy-mode
 "w"       'evil-forward-word-begin
 "y"       'vterm-copy-mode-done
 "DEL"     'my/evil-scroll-up
 "SPC"     'my/evil-scroll-down
 "RET"     'vterm-copy-mode-done
 )

(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key global-map [xterm-paste] #'my/xterm-paste)
            ))

(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function #'(lambda(_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
               #'(lambda(str) (vterm-send-string str t))))
            (apply orig-fun args)))
    (apply orig-fun args)))

(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

(defun my/vterm-ivy-yank ()
  (interactive)
  (ivy-read "yank: " my-vterm-yank-collection 
            :action #'my/vterm-insert-snippet))

(defun my/vterm-insert-snippet (snippet)
  (vterm-send-string snippet)
  (setq my-vterm-yank-collection
        (cons snippet (remove snippet my-vterm-yank-collection))))

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
             ((eq base 'return) (vterm-send-return) (message "done") (throw :exit nil))
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
             ((= base ?m) (vterm-send-return) (message "done") (throw :exit nil))
             (t nil)))
           (t nil)))
        (message "======insert=======insert======insert======")
          ))))

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
  (vterm--update vterm--term " " nil nil nil)
  (sleep-for 0 10)
  (vterm-send-backspace)
  (evil-emacs-state))

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
  (vterm-send-key "q"))

(defun my/vterm-send-y ()
  (interactive)
  (vterm-send-key "y"))

(defun my/vterm-send-C-r ()
  (interactive)
  (vterm-send-C-r)
  (my/vterm-insert-state))

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
