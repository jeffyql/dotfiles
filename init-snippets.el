(defhydra hydra-macro (:hint nil :color pink :pre 
                             (when defining-kbd-macro
                                 (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register     
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))

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
      (when newline-and-indent
        (indent-according-to-mode)))

(defun open-previous-line (arg)
      "Open a new line before the current one. 
     See also `newline-and-indent'."
      (interactive "p")
      (beginning-of-line)
      (open-line arg)
      (when newline-and-indent
        (indent-according-to-mode)))

(provide 'init-snippets)
