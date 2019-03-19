(use-package conda
   :init
   (custom-set-variables '(conda-anaconda-home "~/miniconda3"))
   :config
   (conda-env-autoactivate-mode t)
   )

(use-package python-x)

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)

(setq python-shell-completion-native-enable nil)

(define-key python-mode-map (kbd "M-n") 'python-forward-fold-or-section)
(define-key python-mode-map (kbd "M-p") 'python-backward-fold-or-section)

(my-mf-def
  :states '(normal motion visual) 
  :keymaps 'python-mode-map
  "a"  'tmux-ipython-conda-env-activate
  "b"  'tmux-ipython-send-to-end
  "c"  'tmux-ipython-send-buffer
  "e"  'my/conda-env-activate
  "f"  'tmux-ipython-send-defun
  "i"  'tmux-ipython-start-existing
  "m"  'counsel-tmux-ipython-send-magic
  "n"  'tmux-ipython-start-notebook
  "r"  'tmux-ipython-reset
  "t"  'tmux-ipython-send-from-beginning
  "v"  'tmux-ipython-print
  )

(my-s-def
  :states '(normal motion visual) 
  :keymaps 'python-mode-map
  "s"  'tmux-ipython-send-region
  )

(defvar tmux-ipython-magic-list
  '(
    "%history"
    "%reset -f"
    ))
  
(defun counsel-tmux-ipython-send-magic ()
  (interactive)
  (ivy-read "Select Magic: "
            tmux-ipython-magic-list
            :action (lambda (x) (tmux-run-key x))
            :caller 'counsel-tmux-ipython-send-magic
            ))

(defun tmux-ipython-reset ()
  (interactive)
  (when (y-or-n-p "reset ipython?")
    (tmux-run-key "%reset -f")))

(defun my/conda-env-activate (&optional arg)
  (interactive "P")
  (let* ((env-name conda-env-current-name))
    (when (or (not env-name)
              (y-or-n-p (concat "environment is set to: ["
                                env-name
                                "]. change to a new environment?")))
      (ivy-read "Select a conda environment: "
                (conda-env-candidates)
                :action (lambda (x)
                          (conda-env-activate x)
                          (lsp-restart-workspace))
                :caller 'my/conda-env-activate
            ))
    (tmux-ipython-conda-env-activate)
    (if (y-or-n-p "login to the latest jupyter notebook?")
        (tmux-ipython-start-notebook))))

(defun tmux-ipython-check-pane ()
  (unless (tmux-pane-1-exist-p)
      (error "pane 1 doesn't exist"))
  (if (tmux-python-console-p)
      (if (yes-or-no-p "quit current ipython shell? ")
          (tmux-run-key "exit")
        (error "set conda env not allowed inside an ipython shell"))))

(defun tmux-ipython-conda-env-activate ()
  (interactive)
  (let ((env conda-env-current-name))
    (unless (stringp env)
      (error "conda env for current buffer not set"))
    (tmux-ipython-check-pane)
    (tmux-run-key (concat "conda activate " env))))

(defun tmux-ipython-start-notebook ()
  (interactive)
  (tmux-ipython-check-pane)
  (tmux-run-key "jupyter notebook & ")
  (tmux-run-key "C-m"))


(defun tmux-ipython-run ()
  (interactive)
  (tmux-ipython-check-pane)
  (let ((kernel-id (read-string "kernel id: ")))
    (tmux-run-key "jupyter console --existing " kernel-id)))

(provide 'init-python)
