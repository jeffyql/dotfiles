(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))
(defun copy-from-osx ()
(shell-command-to-string "pbpaste"))

(defun my/xterm-paste (event)
  "Handle the start of a terminal paste operation."
  (interactive "e")
  (unless (eq (car-safe event) 'xterm-paste)
    (error "xterm-paste must be found to xterm-paste event"))
  (let* ((inhibit-read-only t)
         (pasted-text (nth 1 event))
         (interprogram-paste-function (lambda () pasted-text)))
    (if (eq major-mode 'vterm-mode)
        (vterm-yank)
      (yank))))


;;;;; escape key for terminal mode
(defvar my-esc-map nil)

(defun my-esc (map)
  (if (and (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for 0.1))
      [escape]
    map))

(defun my-init-esc ()
  (let ((term (frame-terminal)))
    (when (and (equal (terminal-live-p term) 't)
               (not (terminal-parameter term 'my-esc-map)))
      (let ((my-esc-map (lookup-key input-decode-map [?\e])))
        (set-terminal-parameter term 'my-esc-map my-esc-map)
        (define-key input-decode-map [?\e]
          `(menu-item "" ,my-esc-map :filter ,#'my-esc))))))

(add-hook 'minibuffer-setup-hook 'my-init-esc)

(provide 'init-terminal)
