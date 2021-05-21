(add-to-list 'load-path "~/org-roam/")
(load-library "org-roam")
(setq org-roam-directory (file-truename my-org-dir))
(setq org-roam-file-extensions '("org"))
(setq org-id-link-to-org-use-id t)
(setq org-roam-node-display-template
        "${title:*}  ${tags:20}")
;; (setq org-roam-completion-everywhere t)

(setq org-roam-node-display-template
  "${title:80}  ${file:9} ${tags:20}")

;; (setq org-roam-capture-templates
;;   	  '(("n" plain
;;   	     "%?"
;; 	     :if-new (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org"
;; 				            "#+TITLE: ${title}\n")
;;   	     :unnarrowed t)
;;   	    ))

(defun my/capture-a-reminder ()
  (interactive)
  (org-roam-capture nil "d"))
(defun my/org-roam-capture (&optional goto keys)
  "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'."
  (interactive "P")
  (let ((node (org-roam-node-read)))
    (org-roam-capture- :goto goto
                       :keys keys
                       :node node
                       :props '(:immediate-finish nil))))
(org-roam-setup)

;; (require 'org-roam-doctor)

(define-key global-map (kbd "C-c n /") #'org-roam-node-find)
(define-key global-map (kbd "C-c n c") #'org-roam-capture)
(define-key global-map (kbd "C-c n i") #'org-roam-node-insert)
(define-key global-map (kbd "C-c n r") #'org-roam-buffer-toggle)

(defun my/org-id-update-org-roam-files ()
  "Update Org-ID locations for all Org-roam files."
  (interactive)
  (org-id-update-id-locations (org-roam--list-all-files)))

(defun my/org-id-update-id-current-file ()
  "Scan the current buffer for Org-ID locations and update them."
  (interactive)
  (org-id-update-id-locations (list (buffer-file-name (current-buffer)))))

(defun my/replace-file-with-id-link ()
  "Replaces file links with ID links where possible in current buffer."
  (interactive)
  (let (path desc)
    (org-with-point-at 1
      (while (re-search-forward org-link-bracket-re nil t)
        (setq desc (match-string 2))
        (when-let ((link (save-match-data (org-element-lineage (org-element-context) '(link) t))))
          (when (string-equal "file" (org-element-property :type link))
            (setq path (expand-file-name (org-element-property :path link)))
            (replace-match "")
            (insert (org-roam-format-link path desc))))))))

(defun my/org-roam-node-find ()
  "Find and open an Org-roam node by its title or alias.
INITIAL-INPUT is the initial input for the prompt.
FILTER-FN is the name of a function to apply on the candidates
which takes as its argument an alist of path-completions.
If OTHER-WINDOW, visit the NODE in another window."
  (interactive)
  (let ((node (my/org-roam-node-read)))
    (if (org-roam-node-file node)
        (org-roam-node-visit node))
      (org-roam-capture-
       :node node
       :props '(:finalize find-file))))

(defun my/org-roam-node-read ()
  (let* ((nodes (org-roam-node--completions))
         (prompt "node: ")
         (list (mapcar 'car nodes))
         (selected (ivy-read prompt list
                         :caller 'my/org-roam-node-read
                         )
               ))
    (cdr (assoc selected nodes))
    ))

;; (dolist (file (org-roam--list-all-files))
;;   (with-current-buffer (or (find-buffer-visiting file)
;;                            (find-file-noselect file))
;;     (org-with-point-at 1 (org-id-get-create))
;;     (save-buffer)))

;; (org-roam-db-build-cache)

;; (dolist (file (org-roam--list-all-files))
;;   (with-current-buffer (or (find-buffer-visiting file)
;;                            (find-file-noselect file))
;;     (my/replace-file-with-id-link)
;;     (save-buffer)))

;; (org-roam-db-build-cache)
(provide 'init-org-roam)
