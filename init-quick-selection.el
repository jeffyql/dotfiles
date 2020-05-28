(require 'cl-lib)

(defun my/lists-to-alist (group-names-list)
  (mapcar (lambda (x) (let ((l (symbol-value x))) (cons (mapconcat 'identity l " ") x))) group-names-list))

(defun my/list-to-indexed-string (group-name)
  (let ((counter 0))
    (mapconcat (lambda (e) (concat "[" (number-to-string (cl-incf counter)) "]:" e)) group-name " ")))

(defun my/quick-selection-select-group (group-names-list)
  (let ((group-alist
         (my/lists-to-alist group-names-list))
        group-name)
    (ivy-read "Select group: " (mapcar 'car group-alist)
              :action (lambda (l) (setq group-name
                                        (cdr (assoc l group-alist))))
              :caller 'my/quick-selection-select-group)
    group-name))

(defun my/quick-selection-select-item (group-name)
  (let ((counter 0)
        (group (symbol-value group-name))
        len range prompt selected-num)
    (setq len (length group)
          range (mapcar (lambda (e) (string-to-char (number-to-string (cl-incf counter)))) (make-list len nil))
          prompt (my/list-to-indexed-string group)
          selected-num (char-to-string (read-char-choice prompt range)))))
