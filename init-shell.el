(defun ivy-kill-selected (dummy)
  (interactive)
  (let ((cmd-str (if (string-match url-regex default-directory) ;;remote
                      "sudo kill -9 " "kill -9 "))
        proc-line proc-info name pid command)
    (cl-loop for proc-line in ivy--old-cands
             do
             (setq proc-info (split-string proc-line)
                   name (car proc-info)
                   pid (nth 1 proc-info)
                   command (truncate-string-to-width (nth 10 proc-info) 128))
             (when (y-or-n-p (concat name " " pid " " command)) 
                 (shell-command (concat cmd-str pid)))))
    (message "done"))

(defun kill ()
  (interactive)
  (let* ((url-regex "^/\\(.+?\\):\\(.+?\\)\\([:|]\\)\\(.+\\)")
         (cmd-str (if (string-match url-regex default-directory) ;;remote
                      "ps auxfww" "ps aux"))
         proc-list)
    (shell-command cmd-str "ps")
    (with-current-buffer "ps"
      (setq proc-list (cdr (split-string (buffer-string) "\n" t))))
    (kill-buffer "ps")
    (ivy-read "select process to kill: " proc-list
              :action #'ivy-kill-selected)))

(defvar display-buffer-name "*display*")

(defun my/ivy-display ()
  (pop-to-buffer display-buffer-name)
  (delete-other-windows)
  (swiper))

(defun display-cmd-history ()
  (interactive)
  (let ((lines (number-to-string (read-number "Number of lines: "))))
    (shell-command (concat "cat ~/.bash_history | tail -" lines) display-buffer-name)
    (my/ivy-display)
  ))

(defun display-manual ()
  (interactive)
  (let ((search (read-string "Search term: ")))
    (shell-command (concat "man -P cat " search " | col -b") display-buffer-name)
    (my/ivy-display)
    ))

(defun display-ps ()
  (interactive)
   (let* ((url-regex "^/\\(.+?\\):\\(.+?\\)\\([:|]\\)\\(.+\\)")
          (cmd-str (if (string-match url-regex default-directory) ;;remote
                      "ps auxfww" "ps aux")))
     (shell-command cmd-str display-buffer-name)
     (my/ivy-display)))

(defun display-top ()
  (interactive)
   (let* ((url-regex "^/\\(.+?\\):\\(.+?\\)\\([:|]\\)\\(.+\\)")
          (cmd-str (if (string-match url-regex default-directory) ;;remote
                  "top -b -n 1" "top -l 1")))     
     (shell-command cmd-str display-buffer-name)
     (pop-to-buffer display-buffer-name)
     (unless (one-window-p)
       (delete-other-windows))
     ))

(provide 'init-shell)
