(setq-default tramp-default-method "scp")

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

;(docker-global-mode) 

(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/bash")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
    (around dotemacs-completion-docker activate)
    "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\"
    returns a list of active Docker container names, followed by colons."
    (if (equal (ad-get-arg 1) "/docker:")
        (let* ((dockernames-raw
        (shell-command-to-string
        "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
            (dockernames (cl-remove-if-not
                         #'(lambda (dockerline) (string-match ":$" dockerline))
                         (split-string dockernames-raw "\n"))))
          (setq ad-return-value dockernames))
          ad-do-it))

;; Example:  /ssh:jli@10.100.19.64|docker:jli@ba4ad7f44cf6:/usr/local/etc/log:

(provide 'init-tramp)
