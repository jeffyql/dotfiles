(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; (use-package org
;;   :ensure org-plus-contrib
;;   :pin org)

;;; Document structure
(setq org-M-RET-may-split-line nil)

;;; id
(require 'org-id)

;;; link
(setq org-file-apps
      (quote
       (
        (auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . default)
        (directory . emacs)
        ("\\.log\\'" . emacs)
        ("\\.doc\\'" . default)
        ("\\.docx\\'" . default)
      )))

;;; snipets
(defun org-goto-first-child-cmd ()
  (interactive)
  (org-goto-first-child))

(defun outline-back-to-heading-cmd ()
  (interactive)
  (outline-back-to-heading))

(defun org-insert-prompt ()
  (interactive)
  (insert "$ "))

(defun org-goto-first-child-cmd ()
  (interactive)
  (org-goto-first-child))

(defun outline-back-to-heading-cmd ()
  (interactive)
  (outline-back-to-heading))


;; save archive file
(add-hook 'org-archive-hook 'org-save-all-org-buffers)

;; (define-key org-src-mode-map "q" 'org-edit-src-exit)
;; (defhydra hydra-org-goto (:color blue)
;;   "Org open files"
;;   ("b"   (lambda () (interactive) (dired my-org-babel-dir)) "babel dir")
;;   ("d"   (lambda () (interactive) (find-file my-org-db-dir)) "db dir")
;;   ("l"   (lambda () (interactive) (find-file my-org-code-bookmark-file)) "emacs_link")
;;   ("n"   (lambda () (interactive) (dired my-org-notes-dir)) "notes dir")
;;   ("o"   (lambda () (interactive) (dired my-org-dir)) "org dir")
;;   ("p"   (lambda () (interactive) (find-file (concat my-org-notes-dir "proj.org"))) "current proj")
;;   ("B"   (lambda () (interactive) (find-file (concat my-org-db-dir "bookmark.org"))) "bookmark")
;;   )

(defun my/org-insert-file-tags ()
  (beginning-of-line)
  (if (and (looking-at-p "[[:space:]]*$") (org-before-first-heading-p))
      (insert "#+FILETAGS: ")
      ))

(defun my/org-remove-file ()
  (interactive)
  (org-remove-file)
  (kill-this-buffer)
  (if org-agenda-files
      (org-cycle-agenda-files)
    (message "no more org agenda files")
    (setq hydra-deactivate t)))

(defun body-pre-org-cycle-agenda-files ()
  (if org-agenda-files
      (org-cycle-agenda-files)
    (message "no org agenda files, exit")
    (setq hydra-deactivate t)
    ))
  
;; (defhydra hydra-org-cycle-agenda-files (:body-pre (body-pre-org-cycle-agenda-files))
;;   "org cycle agenda files"
;;   ("RET" org-cycle-agenda-files "cycle")
;;   ("a"   org-agenda-file-to-front "add to front")
;;   ("x"   my/org-remove-file "delete")
;;   )

(face-spec-reset-face 'org-block-begin-line)
(face-spec-reset-face 'org-block-end-line)

;(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; (add-hook 'org-capture-prepare-finalize-hook
;;           (lambda ()
;;             (ignore-errors 
;;               (outline-back-to-heading)
;;               (org-set-tags nil t))))

;; (add-hook 'org-capture-mode-hook 'evil-insert-state)


;; (defun my/capture-a-reminder ()
;;   (interactive)
;;   (org-capture nil "i"))

(defun my/ivy-regex-org-heading (str)
  (ivy--regex-ignore-order (concat "^\\* " str)))

(defun my/find-org-file ()
  (interactive)
  (let* ((projectile-project-root my-org-dir))
    (projectile-find-file)))

(defun my/counsel-rg-org-search (&optional arg)
  (interactive "P")
  (if (equal arg '(16))
      (my/find-org-file)
    (let ((ivy-re-builders-alist
           (if (equal arg '(4))
               '((t . ivy--regex-ignore-order))
             '((t . my/ivy-regex-org-heading)))))
      (counsel-rg nil my-org-dir nil))))

(defun my/org-open-link-this-window ()
  (interactive)
  (let ((org-link-frame-setup
         (quote ((file . find-file)))))
    (my/org-open-link)
    ))

(defun my/org-open-link ()
   (interactive)
   (save-excursion
     (if (org-in-regexp org-any-link-re)
         (org-open-at-point)
       (org-next-link))
       ))

 (defun my/org-store-link-to-current-line ()
   (interactive)
   (let* ((file (buffer-file-name (or (buffer-base-buffer) (current-buffer))))
          (line-num (number-to-string (line-number-at-pos nil t)))
          (beg (if (use-region-p) (region-beginning) (point-at-bol)))
          (end (if (use-region-p) (region-end) (point-at-eol)))
          (text (buffer-substring beg end))
          (link-text (concat "file:" file "::" line-num))
          (desc (concat (file-name-nondirectory file) ":" line-num ":"))
          (link (org-make-link-string link-text desc)))
     (kill-new (concat "- " link " " text))
     (evil-first-non-blank)))

 (defun my/org-store-cc-defun-link ()
   (interactive)
   (let* ((buf (or (buffer-base-buffer) (current-buffer)))
          (file-name (buffer-file-name buf))
          (line-num (number-to-string (line-number-at-pos nil t)))
          (name-and-limits (c-defun-name-and-limits nil))
          (func-name (if name-and-limits (car name-and-limits)))
          (proj-root (projectile-project-root))
          file-relative-name text link-text desc link)
     (unless func-name
       (error "unable to capture function name"))
     (unless proj-root
       (error "project root not found"))
     (save-excursion
       (goto-char (cadr name-and-limits))
       (setq link-text (buffer-substring (point-at-bol) (point-at-eol))))
     (setq link-text (concat "file:" file-name "::" link-text)
           desc (concat "<FUNC>: " func-name)
           link (org-make-link-string link-text desc))
     (setq file-relative-name (file-relative-name file-name proj-root)
           text (concat "- " link " (File: " file-relative-name " Line: " line-num ")\n  "))
     (kill-new text)
     (evil-first-non-blank)))

 (defun my/org-store-cc-line-link ()
   (interactive)
   (let* ((buf (or (buffer-base-buffer) (current-buffer)))
          (file (buffer-file-name buf))
          (line-num (number-to-string (line-number-at-pos nil t)))
          (beg (if (use-region-p) (region-beginning) (point-at-bol)))
          (end (if (use-region-p) (region-end) (point-at-eol)))
          (text (buffer-substring beg end))
          (link-text (concat "file:" file "::" line-num))
          (name-and-limits (c-defun-name-and-limits nil))
          (limits (cdr name-and-limits))
          (point-bol (c-point 'bol))
          desc)
     (if limits
         (setq line (format "%s" (1+ (count-lines (car limits) (max point-bol (car limits)))))
               total (format "%s" (count-lines (car limits) (cdr limits)))
               desc (concat " " line "/" total " (Line: " line-num ")")
               link (org-make-link-string link-text desc))
       (error "not inside a function"))
     (kill-new (concat "- " link " " text))
     (evil-first-non-blank)))

(defun my/org-bookmark (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (org-capture nil "l")
    (let ((link-text
          (cond
           ((derived-mode-p 'prog-mode)
            (my/get-org-link-to-source-code-text))
           (t
            (my/get-org-link-to-current-line)))))
      (with-current-buffer (find-file-noselect my-org-code-bookmark-file)
        (goto-char (point-max))
        (insert ?\n)
        (insert link-text)
        (save-buffer)
        (kill-new link-text)
        (message "link saved")))
    ))

(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (shell . t)
                               ;(restclient . t)
                               (sql . t)
                               )
                             )

(defun my/org-store-headline-link ()
  (interactive)
  (let* ((url (concat "id:" (org-id-get-create)))
         (link (org-make-link-string url "-->")))
    (save-buffer)
    (kill-new link)))

(defun my/org-store-headline-link-1 ()
  (interactive)
  (let* ((url (concat "id:" (org-id-get-create)))
         (link (org-make-link-string url "-->"))
         (tags (org-get-tags-at)))
    (save-buffer)
    (kill-new (concat (org-get-heading t t)
                      " "
                      link
                      (if tags (concat " :" (mapconcat 'identity tags ":") ":"))))
    ))

(defun my/org-capture-heading-link ()
  (with-current-buffer (org-capture-get :original-buffer)
    (my/org-heading-link)))

;; (defhydra hydra/org-mac-link (:color blue)
;;   ("c" org-mac-chrome-get-frontmost-url "chrome")
;;   ("f" org-mac-finder-item-get-selected "finder")
;;   ("m" org-mac-message-insert-selected "mail")
;;   )

(defun my/org-goto-reminders ()
  (interactive)
  (find-file my-org-todo-file)
  (goto-char (point-min))
  (org-overview)
  )

(defun my/org-save-link-to-kill-ring ()
  (interactive)
  (let* ((buf (or (buffer-base-buffer) (current-buffer)))
         (beg (if mark-active (region-beginning) (point-at-bol)))
         (end (if mark-active (region-end) (point-at-eol)))
         (text (buffer-substring beg end))
         (file (buffer-file-name buf))
         line-end line-num line-text name cplink header-text)
    (setq cpltxt (concat "file:" file "::" text)
          cpltxt (org-make-link-string cpltxt
                                       (read-string "Link Description: " nil nil "->")))
    (kill-new cpltxt)))

(defun my/org-retrieve-url-from-point ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)
      (kill-new text))))

(defun my/org-save-source-code-link-and-text ()
  (interactive)
  (my/org-save-source-code-link t))

(defun my/org-goto-file-other-window ()
  (interactive)
  (let (beg end file-name)
    (save-excursion
      (if (org-in-item-p)
          (beginning-of-line)
        (org-beginning-of-item))
      (looking-at org-list-full-item-re)
      (setq pos (car (last (match-data))))
      (goto-char pos)
      (skip-chars-forward " \t")
      (setq beg (point)
            end (line-end-position)))
    (setq file-name (buffer-substring-no-properties beg end))
    (unless (file-exists-p file-name)
      (error "not a file name of an existing file"))
    (find-file-other-window file-name)
    ))

(defun my/imenu-compare-function (str fun)
  (string-match (concat str "$") (car fun)))

(defun my/org-bookmark-goto-definition ()
  (interactive)
  (let (beg end file-name str index-item)
    (save-excursion
      (unless (symbol-at-point)
        (error "symbol not found at point"))
      (setq str (thing-at-point 'symbol))
      (set-text-properties 0 (length str) nil str)
      (forward-line -1)
      (if (org-in-item-p)
          (beginning-of-line)
        (org-beginning-of-item))
      (looking-at org-list-full-item-re)
      (setq pos (car (last (match-data))))
      (goto-char pos)
      (skip-chars-forward " \t")
      (setq beg (point)
            end (line-end-position)))
    (setq file-name (buffer-substring-no-properties beg end))
    (unless (file-exists-p file-name)
      (error "not a file name of an existing file"))
    (find-file-other-window file-name)
    (setq items (imenu--make-index-alist)
          items (delete (assoc "*Rescan*" items) items))
    (setq find (cl-member str items :test #'my/imenu-compare-function))
    (if find
        (imenu (car find)))
    ))

(defun org-eldoc-load ()
  "Set up org-eldoc documentation function."
  (interactive)
  (cond
   ((boundp 'eldoc-documentation-strategy)
    (setq-local eldoc-documentation-strategy
               #'org-eldoc-documentation-function))
   ((boundp 'eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions
             #'org-eldoc-documentation-function nil t))
   (t (setq-local eldoc-documentation-function
                 #'org-eldoc-documentation-function))))

(provide 'init-org)
