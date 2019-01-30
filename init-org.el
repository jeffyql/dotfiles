(require 'org-id)
;(require 'helm-org-rifle)

(setq org-agenda-files (apply 'append
			      (mapcar
			       (lambda (directory)
				 (directory-files-recursively
				  directory org-agenda-file-regexp))
			       '("~/org/db/" "~/org/notes/"))))

(defun org-goto-first-child-cmd ()
  (interactive)
  (org-goto-first-child))

(defun outline-back-to-heading-cmd ()
  (interactive)
  (outline-back-to-heading))

(define-key org-mode-map (kbd "M-b") 'outline-back-to-heading-cmd) 
(define-key org-mode-map (kbd "M-h") 'outline-up-heading) 
(define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level) 
(define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level) 
(define-key org-mode-map (kbd "M-l") 'org-goto-first-child-cmd) 
(define-key org-mode-map (kbd "M-L") 'org-metaright)
(define-key org-mode-map (kbd "M-H") 'org-metaleft)
(define-key org-mode-map (kbd "M-K") 'org-metaup)
(define-key org-mode-map (kbd "M-J") 'org-metadown)
(define-key org-mode-map (kbd "M-F") 'org-shiftmetaright)
(define-key org-mode-map (kbd "M-B") 'org-shiftmetarleft)
(define-key org-mode-map (kbd "M-U") 'org-metaup)
(define-key org-mode-map (kbd "M-D") 'org-metadown)
(define-key org-mode-map (kbd "C-j") 'org-meta-return)
(define-key org-mode-map (kbd "M-RET") 'org-insert-todo-heading)

(general-define-key :states '(normal)
                    :keymaps 'org-mode-map
                    :prefix ","
                    "a"  'org-archive-subtree
                    "b"  (lambda () (interactive) (outline-back-to-heading))
                    "c"  'org-ctrl-c-ctrl-c
                    "d"  'org-cut-special
                    "e"  'org-export-dispatch
                    "fd" 'my/org-bookmark-goto-definition
                    "ff" 'my/org-goto-file-other-window
                    "h"  'org-navigate-up/body
                    "j"  'org-navigate-next/body
                    "k"  'my/org-store-headline-link-1
                    "h"  'my/org-store-headline-link
                    "n"  'org-next-visible-heading
                    "p"  'org-previous-visible-heading
                    "s"  'org-save-all-org-buffers
                    "H"  'org-shiftmetaleft
                    "<s-down>"  'org-metadown
                    "<s-right>"  'org-metaright
                    "K"  'org-metaup
                    "L"  'org-shiftmetaright
                                        ;"b"  'org-previous-link
                                        ; "f"  'org-next-link
                                        ; "g"  'org-mark-ring-goto
                    "m"  'org-mac-grab-link

                    "o"  'my/org-open-link
                    "i"  'my/org-open-link-this-window
                    "y"  'org-cliplink
                    ";"  'my/org-store-headline-link
                    "'"  'org-insert-link
                                        ; "w"  'my/org-retrieve-url-from-point
                    "u"  'outline-up-heading
                    "U"  'org-babel-load-file
                    "."  'org-time-stamp-inactive
                    "RET" 'org-meta-return
                    "&"   'org-mark-ring-goto
                    "-"   'org-ctrl-c-minus
                    "g"   'org-agenda-file-to-front
                    )

(defun org-goto-first-child-cmd ()
  (interactive)
  (org-goto-first-child))

(defun outline-back-to-heading-cmd ()
  (interactive)
  (outline-back-to-heading))


(add-to-list 'org-file-apps '(directory . emacs) t)
(add-to-list 'org-file-apps '("\\.log\\'" . emacs) t)
(add-to-list 'org-file-apps '("\\.docx\\'" . default) t)

;; save archive file
(add-hook 'org-archive-hook 'org-save-all-org-buffers)

(define-key org-src-mode-map "q" 'org-edit-src-exit)
(defhydra hydra-org-goto (:color blue)
  "Org open files"
  ("b"   (lambda () (interactive) (dired my-org-babel-dir)) "babel dir")
  ("d"   (lambda () (interactive) (find-file my-org-db-dir)) "db dir")
  ("l"   (lambda () (interactive) (find-file my-org-code-bookmark-file)) "emacs_link")
  ("n"   (lambda () (interactive) (dired my-org-notes-dir)) "notes dir")
  ("o"   (lambda () (interactive) (dired my-org-dir)) "org dir")
  ("p"   (lambda () (interactive) (find-file (concat my-org-notes-dir "proj.org"))) "current proj")
  ("B"   (lambda () (interactive) (find-file (concat my-org-db-dir "bookmark.org"))) "bookmark")
  )

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
  
(defhydra hydra-org-cycle-agenda-files (:body-pre (body-pre-org-cycle-agenda-files))
  "org cycle agenda files"
  ("RET" org-cycle-agenda-files "cycle")
  ("a"   org-agenda-file-to-front "add to front")
  ("x"   my/org-remove-file "delete")
  )

(face-spec-reset-face 'org-block-begin-line)
(face-spec-reset-face 'org-block-end-line)

;(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'org-capture-prepare-finalize-hook
          (lambda ()
            (ignore-errors 
              (outline-back-to-heading)
              (org-set-tags nil t))))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(setq org-capture-templates
      '(
        ("i" "Inbox" entry (file my-org-reminders-file) "* TODO %?\n  %t" :prepend t)
        ("l" "emacs link topic" entry (file my-org-code-bookmark-file) "* %?\n")
        ))


(defun my/capture-a-reminder ()
  (interactive)
  (org-capture nil "i"))

(defun my/org-counsel-rg-function (string)
  (if (< (length string) 3)
      (counsel-more-chars)
    (let ((default-directory (ivy-state-directory ivy-last))
          (base-command (concat "rg -i --no-heading --line-number --color never -g \"*.org\" %s ."))
          (regex (counsel-unquote-regex-parens
                  (concat "^\\*+ +.*?" (ivy--regex string)))))
                                        ; also works: (ivy--regex (concat "^\\* " string))))))
      (let ((rg-cmd (format base-command
                            (concat " -- "
                                    (shell-quote-argument regex)))))
        (counsel--async-command rg-cmd))
      nil))
  )

(defun my/counsel-rg-org-search (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (counsel-rg nil my-org-dir)
    (counsel-require-program (car (split-string counsel-rg-base-command)))
    (ivy-set-prompt 'counsel-rg counsel-prompt-function)
    (let ((default-directory my-org-dir))
      (ivy-read "org heading search: "
                (lambda (string)
                  (my/org-counsel-rg-function string))
                :initial-input nil
                :dynamic-collection t
                :keymap counsel-ag-map
                :history 'counsel-git-grep-history
                :action #'counsel-git-grep-action
                :unwind (lambda ()
                          (counsel-delete-process)
                          (swiper--cleanup))
                :caller 'my/counsel-rg-org-search))))

(defun my/find-org-file (&optional open-another-window)
  (interactive "P")
  (let* ((ffip-project-root my-org-dir)
         (ffip-rust-fd-extra-opts "-e org"))
    (ffip open-another-window)))

;;find . -name *.org -printf "%T+\t%p\n" | sort
;;find %s ! -readable -prune -o -iname \"%s*\" -print"
;; (defun counsel-find-function (str)
;;   (let ((cmd
;;             (concat (format "find %s " my-org-dir) "-name '*.org' -printf \"%T+\\t%p\n\""))))
;;       (message "%s" cmd)
;;       (counsel--async-command cmd))
;;     '("" "working..."))

;;;###autoload
;; (defun counsel-find (&optional initial-input)
;;   "Use GNU find, counsel and ivy  to present all paths
;;    in a directory tree that match the REGEX input"
;;   (interactive)
;;   (ivy-read "Find: " #'counsel-find-function
;;             :initial-input initial-input
;;             :dynamic-collection t
;;             :history 'counsel-find-history
;;             :action (lambda (file)
;;                       (with-ivy-window
;;                         (when file
;;                           (find-file file))))
;;             :unwind #'counsel-delete-process
;;             :caller 'counsel-find))

(counsel-set-async-exit-code 'counsel-find 1 "Nothing found")
(defun my/org-open-link-this-window ()
  (interactive)
  (let ((org-link-frame-setup
         (quote ((file . find-file)))))
    (my/org-open-link)
    ))

(defun my/org-open-link ()
  (interactive)
  (unless (org-in-regexp org-any-link-re)
    (org-next-link))
  (when (org-in-regexp org-any-link-re) 
    (org-open-at-point)
    ))

(defun my/get-org-link-to-current-line ()
  (let* ((file (buffer-file-name (or (buffer-base-buffer) (current-buffer))))
         (text (buffer-substring (point-at-bol) (point-at-eol)))
         (link-text (substring text 0 27)) 
         cpltxt)
    (save-excursion
      (widen)
      (setq line-num (number-to-string (line-number-at-pos))))
    (setq cpltxt (concat "file:" file "::" link-text)
          cpltxt (org-make-link-string cpltxt  "->"))
    (concat "  - " (file-name-nondirectory file)
            "\n    line[" line-num "] " cpltxt "  " text)))
  
(defun my/get-org-link-to-source-code-text ()
  (let* ((buf (or (buffer-base-buffer) (current-buffer)))
         (beg (if mark-active (region-beginning) (point-at-bol)))
         (end (if mark-active (region-end) (point-at-eol)))
         (text (buffer-substring beg end))
         (file (buffer-file-name buf))
         line-end line-num line-text name cplink header-text)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char beg)
        (setq line-end (point-at-eol)
              line-text (buffer-substring beg line-end)
              line-num (number-to-string (line-number-at-pos))
              func-name (which-function))))
    (setq cpltxt (concat "file:" file "::" text)
          cpltxt (org-make-link-string cpltxt "->"))
    (setq header-text (concat (which-function) " (" (my/two-level-file-path file) ")"))
    (concat "  - " header-text
            "\n    line[" line-num "] " cpltxt "  " line-text)))

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

(defhydra hydra/org-mac-link (:color blue)
  ("c" org-mac-chrome-get-frontmost-url "chrome")
  ("f" org-mac-finder-item-get-selected "finder")
  ("m" org-mac-message-insert-selected "mail")
  )

(defun my/org-goto-reminders ()
  (interactive)
  (find-file my-org-reminders-file)
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

(provide 'init-org)
