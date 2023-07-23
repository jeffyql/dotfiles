(general-def 'normal 'python-mode-map
  "<return>"     'xref-find-definitions-other-window
  "RET"          'xref-find-definitions-other-window
  "M-p"          'code-cells-move-cell-up
  "M-n"          'code-cells-move-cell-down
  "M-j"          'code-cells-forward-cell
  "M-k"          'code-cells-forward-cell
 )

(my-mc-def 'normal 'python-mode-map
  "c"   'evil-commentary-line
  "e"   'my/start-eglot
  "m"   'code-cells-mark-cell
  "t"   'code-cells-mode
  )

;; https://robbmann.io/posts/emacs-eglot-pyrightconfig/
(defun my/eglot-workspace-config-1 (server)
  (interactive "DEnv: ")
  (let* (;; file-truename and tramp-file-local-name ensure that neither `~' nor
         ;; the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
         ;; absolute directory path.
         (venv-dir (tramp-file-local-name (file-truename "/sshx:jeff@192.168.2.1:/home/jeff/miniconda3/envs/proj1")))

         ;; Given something like /path/to/.venv/, this strips off the trailing `/'.
         (venv-file-name (directory-file-name venv-dir))

         ;; Naming convention for venvPath matches the field for
         ;; pyrightconfig.json.  `file-name-directory' gets us the parent path
         ;; (one above .venv).
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-nondirectory venv-dir))
         (pythonPath (concat venvPath "bin/python3"))) 

      (list (cons :python
                  (list :venvPath vp :venv vn
                        :pythonPath (concat venv "/bin/python3"))))))
(defvar venvPathes
  '(
    "/home/jeff/miniconda3/envs/proj1"
    "/Users/yuanqianli/miniconda3/envs/proj1"
    ))

(defvar venv nil)
(defvar pythonPath nil)

(defun my/start-eglot ()
  (interactive)
  (setq venv (completing-read "Select venvPath: " venvPathes)
        vp (file-name-directory venv)
        vn (file-name-nondirectory venv))
  (setq-default eglot-workspace-configuration
                (list (cons :python
                            (list :venvPath vp :venv vn
                                  :pythonPath (concat venv "/bin/python3")))))
  (call-interactively 'eglot))

(my-m-def 'normal 'python-mode-map
  "s"   'my/python-send-to-repl 
  )
(defun my/python-send-to-repl ()
  (interactive)
  (let ((buf (unless (one-window-p) (window-buffer (next-window))))
        beg end window)
    (unless (bufferp buf)
        (error "next window not found"))
    (with-current-buffer buf
      (unless (eq major-mode 'vterm-mode)
        (error "next window buffer is not a repl buffer")))
    (save-excursion
      (python-nav-beginning-of-statement)
      (cond
       ((python-info-current-defun)
        (setq beg
              (progn
                (end-of-line 1)
                (while (and (or (python-nav-beginning-of-defun)
                                (beginning-of-line 1))
                            (> (current-indentation) 0)))
                (while (and (forward-line -1)
                            (looking-at (python-rx decorator))))
                (and (not (bobp)) (forward-line 1))
                (point-marker))
              end
              (progn
                (or (python-nav-end-of-defun)
                    (end-of-line 1))
                (point-marker))))
       ((python-info-beginning-of-statement-p)
        (setq beg (point)
              end (python-nav-end-of-statement)))))
    (setq cmd-str (buffer-substring-no-properties beg end))
    (with-current-buffer buf
      (vterm-insert cmd-str)
      (vterm-send-return)
      )))

;; (use-package python-x)

;; (setq python-shell-interpreter "python3")

(provide 'init-python)
