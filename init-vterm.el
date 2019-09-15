(use-package vterm
     :load-path "/home/jli/emacs-libvterm"
     :config
     (progn
       (add-to-list 'evil-emacs-state-modes 'vterm-mode)
       (setq vterm-max-scrollback 100000)
       ))


(add-to-list 'vterm-keymap-exceptions "M-:")
(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key vterm-mode-map (kbd "M-:")          #'eval-expression)
            ;; (define-key evil-normal-state-local-map (kbd "ESC") 'my/vterm-toggle-stop)
            ;; (define-key evil-normal-state-local-map [escape] 'my/vterm-toggle-stop)
            (define-key evil-normal-state-local-map (kbd "ESC") 'vterm--self-insert)
            (define-key vterm-mode-map (kbd "C-t") 'my/vterm-send-escape)
            (define-key vterm-mode-map [up] 'vterm-send-up)
            (define-key evil-normal-state-local-map (kbd "a") 'my/vterm-back-to-prompt)
            (define-key evil-normal-state-local-map (kbd "c") 'my/vterm-ctrl-c)
            (define-key evil-normal-state-local-map (kbd "d") 'my/vterm-ctrl-n)
            (define-key evil-normal-state-local-map (kbd "i") 'my/vterm-insert)
            (define-key evil-normal-state-local-map (kbd "p") 'my/vterm-yank)
            (define-key evil-normal-state-local-map (kbd "o") 'my/vterm-toggle-copy-mode)
            (define-key evil-normal-state-local-map (kbd "r") 'my/vterm-ctrl-r)
            (define-key evil-normal-state-local-map (kbd "DEL") 'my/evil-scroll-up)
            (define-key evil-normal-state-local-map [backspace] 'my/evil-scroll-up)
            (define-key evil-normal-state-local-map (kbd "u") 'my/vterm-ctrl-p)
            (define-key vterm-mode-map (kbd "M-x") 'execute-extended-command)
            (define-key vterm-mode-map (kbd "M-:") 'eval-expression)
            ))
 
(defun my/vterm-back-to-prompt ()
  (interactive)
  (vterm--update vterm--term " " nil nil nil)
  (vterm-send-backspace))

(defun my/vterm-toggle-stop ()
  (interactive)
  (if vterm-copy-mode
    (vterm-copy-mode 1)
    (set-cursor-color "DarkMagenta")))

(defun my/vterm-send-escape ()
  (interactive)
  (vterm-send-key "<escape>" nil nil nil))

(defun my/vterm-insert ()
    (interactive)
    (vterm-copy-mode -1)
    (vterm--update vterm--term " " nil nil nil)
    (vterm-send-backspace)
    (evil-emacs-state 1))
(defun my/vterm-kill-whole-line ()
     (interactive)
     (vterm-send-key "a" nil nil t)
     (vterm-send-key "k" nil nil t)
     )
  
   (defun my/vterm-ctrl-c ()
     (interactive)
     (if vterm-copy-mode
         (error "command not effective in copy mode")
       (vterm-send-ctrl-c)
       ))
  
   (defun my/vterm-ctrl-n ()
     (interactive)
     (if vterm-copy-mode
         (error "command not effective in copy mode")
       (vterm-send-key "n" nil nil t)
       ))
  
   (defun my/vterm-ctrl-p ()
     (interactive)
     (if vterm-copy-mode
         (error "command not effective in copy mode")
       (vterm-send-key "p" nil nil t)))
  
   (defun my/vterm-ctrl-r ()
     (interactive)
     (if vterm-copy-mode
         (error "command not effective in copy mode")
       (vterm-send-key "r" nil nil t)
     (evil-emacs-state)))
 
 (defun my/vterm-clear ()
   (interactive)
   (vterm-send-key "l" nil nil t))
 
 (defun my/vterm-yank ()
   (interactive)
   (if vterm-copy-mode
       (error "vterm in copy mode"))
   (vterm-yank))
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

(defun my/vterm-send-region ()
   (interactive)
   (let* (beg end cmd-str no-history)
     (unless my-vterm-selected-buffer
       (error "vterm buffer not selected"))
     (cond
      ((eq major-mode 'python-mode)
       (unless (equal "python" (substring (tmux-pane-0-login-type) 0 6))
         (error "must run in a ipthon shell"))))
     (cond
      ((use-region-p)
       (setq beg (region-beginning)
             end (region-end))
       (deactivate-mark t)
       (unless (= (line-number-at-pos beg) (line-number-at-pos end))
         (setq no-history t)))
      ((and (eq major-mode 'org-mode) (org-in-item-p))
       (save-excursion
         (beginning-of-line)
         (looking-at org-list-full-item-re)
         (setq pos (car (last (match-data))))
         (goto-char pos)
         (if (looking-at "[ \t]*\\(\\$\\|#\\)[ \t]+")
             (goto-char (match-end 0)))
         (setq beg (point)
               end (line-end-position))))
      (t
       (save-excursion
         (setq beg (line-beginning-position)
               end (line-end-position)))))
     (setq cmd-str (buffer-substring-no-properties beg end))
     (with-current-buffer my-vterm-selected-buffer
       (vterm-send-string cmd-str)
       (vterm-send-return)
       (my/vterm-back-to-prompt)
       )))

(defun my/vterm-select-terminal-by-number (num &optional winum)
   (let ((buffer (get-buffer (concat "*vterm-" (int-to-string num) "*")))
         window)
     (unless buffer
       (init-vterms)
       (setq buffer (get-buffer (concat "*vterm-" (int-to-string num) "*"))))
     (if winum
         (progn
           (setq window (winum-get-window-by-number winum))
           (set-window-buffer window buffer)
           (winum-select-window-by-number winum))
       (switch-to-buffer buffer))))

(defun select-vterminal-1 (&optional winum)
  (interactive "P")
  (my/vterm-select-terminal-by-number 1 winum))

(defun select-vterminal-2 (&optional winum)
  (interactive "P")
  (my/vterm-select-terminal-by-number 2 winum))

(defun select-vterminal-3 (&optional winum)
  (interactive "P")
  (my/vterm-select-terminal-by-number 3 winum))

(defun select-vterminal-4 (&optional winum)
  (interactive "P")
  (my/vterm-select-terminal-by-number 4 winum))

(defun select-vterminal-5 (&optional winum)
  (interactive "P")
  (my/vterm-select-terminal-by-number 5 winum))

(defun select-vterminal-6 (&optional winum)
  (interactive "P")
  (my/vterm-select-terminal-by-number 6 winum))

(provide 'init-vterm)
