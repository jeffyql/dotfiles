(defvar my-saved-lists-alist nil)

(add-hook 'kill-emacs-hook #'my/saved-lists-alist-save)

(defun my/saved-lists-to-file (var-symbol file)
  (unless (symbolp var-symbol)
    (error "not a symbol"))
  (unless (file-exists-p file)
    (error "not a file"))
  (let ((buf (find-file-noselect file))
        (list (symbol-value var-symbol)))
    (unless (listp list)
      (error "not a list"))
    (with-current-buffer buf
      (erase-buffer)
      (print (list 'setq var-symbol (list 'quote list)) (point-min-marker))
      (save-buffer))))

(defun my/saved-lists-add-or-remove-element (var-symbol remove)
  (let ((var (symbol-value var-symbol))
        element)
    (unless (listp var)
      (error "not a list"))
    (if remove
        (ivy-read "Yank command element: " var
                :action (lambda (s) (setq var (remove s var))))
      (when (use-region-p)
        (setq beg (region-beginning)
              end (region-end)
              element
              (if (= (line-number-at-pos beg t)  (line-number-at-pos end t))
                  (buffer-substring beg end))))
       (unless element
         (setq element (read-string "add new term: ")))
      (setq var (cons element (remove element var)))
      (when (> (length var) 2000)
        (setq var (butlast var))))
    (set var-symbol var)))

(defun my/saved-lists-select (var-symbol)
  (let ((var (symbol-value var-symbol))
        selected)
    (ivy-read "Select item: " var
              :action #'(lambda (e) (setq selected e)
                               (set var-symbol (cons e (remove e var)))))
    selected))

(defun my/saved-lists-alist-save ()
  (cl-loop for var-name in my-saved-lists-alist do
           (my/saved-lists-to-file (car var-name) (cdr var-name))))


(provide 'init-saved-lists)
