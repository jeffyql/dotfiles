(defun highlight-symbol-at-point-all-windows ()
  "Toggle highlighting of the symbol at point in all windows."
  (interactive)
  (let ((symbol (highlight-symbol-get-symbol)))
    (unless symbol (error "No symbol at point"))
    (if (highlight-symbol-symbol-highlighted-p symbol)
        (save-selected-window
          (cl-dolist (x (window-list))
            (select-window x)
            (highlight-symbol-remove-symbol symbol)))
      (let ((color (highlight-symbol-next-color)))
        (save-selected-window
          (cl-dolist (x (window-list))
            (select-window x)
          (highlight-symbol-add-symbol symbol color))))
      )))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun new-scratch-buffer-new-window (&optional arg)
  "Create a new scratch buffer in a
  new window. I generally take a lot of notes 
  in different topics. For each new topic hit
  C-c C-s and start taking your notes. 
  Most of these notes don't need to be 
  saved but are used like quick post it 
  notes."
  (interactive "P")
  (if (equal arg '(4))
      (let* ((input (read-string "new buffer name: "))
             (name (if (string-empty-p input)
                       "notes"
                     input))
             (buf (generate-new-buffer name))
             (mode (read-string "buffer mode: ")))
        (when (not (string-empty-p mode))
          (setq mode-string  (concat "\\." mode "\\'")
                mode-func (cdr (assoc mode-string auto-mode-alist)))
          (if mode-func
              (with-current-buffer buf
                (funcall mode-func))))
        (pop-to-buffer buf)
        (balance-windows))
    (let ((buf (generate-new-buffer "notes")))
      (pop-to-buffer buf)
      (balance-windows))))

(defun toggle-buffer-coding-system ()
  (interactive)
  (if (eq  buffer-file-coding-system 'utf-8-dos)
      (revert-buffer-with-coding-system 'utf-8-unix)
    (revert-buffer-with-coding-system 'utf-8-dos)))

(defun open-next-line (arg)
      "Move to the next line and then opens a line.
    See also `newline-and-indent'."
      (interactive "p")
      (end-of-line)
      (open-line arg)
      (next-line 1)
      (indent-according-to-mode))

(defun open-previous-line (arg)
      "Open a new line before the current one. 
     See also `newline-and-indent'."
      (interactive "p")
      (beginning-of-line)
      (open-line arg)
      (indent-according-to-mode))

(provide 'init-snippets)
