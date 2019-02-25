(general-define-key
 :states '(motion normal visual)
 :keymaps 'override
 "i"   'my/evil-insert
 "z"   'my/toggle-buffer
 ";"   'avy-goto-word-or-subword-1
 "/"   'evil-ex-search-forward
 ","   'my/switch-window
 ":"   'evil-ex
 "SPC" 'evil-scroll-page-down
 "DEL" 'evil-scroll-page-up
 )

(general-define-key
 :states '(motion normal visual)
 "t"   'universal-argument
 "q"   'my/quit-this-buffer
 "TAB" nil
 ">"   'evil-shift-right-line
 "<"   'evil-shift-left-line
 )

(define-key universal-argument-map "t" 'universal-argument-more)

(general-define-key
 "M-i" 'completion-at-point
 "M-." 'xref-find-definitions
 "M-?" 'xref-find-references
 "<s-right>" nil
 "<s-left>" nil
 "<s-up>" nil
 "<s-down>" nil
 )

(general-create-definer my-return-def
  :prefix "RET")

(my-return-def
 :states 'normal
 "RET"   'open-next-line
 "DEL"   'open-previous-line
 "SPC"   'just-one-space
 )

(general-create-definer my-f-def
  :prefix "f")
(my-f-def
  :states '(normal motion visual) 
  :keymaps 'override
  "a"    'my/ffap
  "c"    'counsel-command-history
  "e"    'my/evil-end-of-line
  "f"    'counsel-find-file
  "d"    'dired
  "g"    'grep
  "h"    'hydra-help/body
  "i"    'counsel-imenu
  "j"    'hydra-recentf/body
  "k"    'rg
  "l"    'my/counsel-rg-at-point
  "m"    'my/swiper
  "o"    'my/counsel-rg-org-search
  "o"    'hydra-org-find/body
  "pa"   'projectile-add-known-project 
  "pk"   'projectile-kill-buffers
  "pr"   'projectile-remove-known-project
  "ps"   'counsel-projectile-switch-project
  "r"    'counsel-mark-ring
  "s"    'hydra-shell-cmd/body
  "s"    'ivy-switch-buffer
  "t"    'my/ripgrep-this-file
  "u"    'counsel-bookmark
  "w"    'tmux-select-active-window
  "y"    'counsel-yank-pop
  "ESC"  'keyboard-quit
  ",c"    'my/swiper-all-c++
  "."    'ivy-resume
  )

(general-create-definer my-g-def
  :prefix "g")
(my-g-def
  :states '(normal motion visual) 
  :keymaps 'override
  "a"    'evil-first-non-blank
  "b"    'evil-scroll-line-to-bottom
  "c"    'new-scratch-buffer-new-window
  "d"    'dired-jump
  "e"    'evil-last-non-blank
  "g"    'evil-goto-first-line
  "h"    (lambda () (interactive) (dired "~"))
  "i"    'ibuffer
  "l"    'evil-goto-line
  "j"    'tmux-goto-dir
  "n"    'evil-next-match
  "o"    'hydra-org-goto/body
  "s"    '(lambda () (interactive) (switch-to-buffer "*scratch*"))
  "t"    'evil-scroll-line-to-top
  ";"    'goto-last-change
  ","    'pop-to-mark-command
  "."    'evil-scroll-line-to-center
  "G"    (lambda () (interactive) (find-file my-org-checklist-file))
  "ESC"  'keyboard-quit
  "SPC"  'my/org-goto-reminders
  "RET"  'hydra-org-cycle-agenda-files/body
  )

(general-create-definer my-m-def
  :prefix "m")
(my-m-def
  :states '(normal motion visual) 
  :keymaps 'override
  "a"    'my/kill-ring-save-symbol-at-point
  "b"    'hydra-buffer/body
  "c"    'hydra-misc/body
  "e"    'my/split-window-vertically
  "fw"   'write-file
  "g"    'magit-status
  "i"    'my/switch-indirect-narrow
  "n"    'my/evil-ex-search-word-forward
  "o"    'occur
  "q"    'delete-window
  "r"    'delete-other-windows
  "s"    'my/save-buffer
  "t"    'hydra-toggle/body
  "v"    'er/expand-region
  "u"    'hydra-kmacro-end-or-call-macro-repeat/body
  "w"    'my/split-window-horizontally
  "x"    'counsel-M-x
  "z"    'delete-frame
  "B"    'balance-windows
  "D"    'save-buffers-kill-emacs
  "F"    'get-buffer-file-name
  "G"    'magit-dispatch-popup
  "M"    'toggle-window-split
  "h"    'hydra-window-left/body
  "j"    'hydra-window-down/body
  "k"    'hydra-window-up/body
  "l"    'hydra-window-right/body
  "T"    'test
  "X"    'shell-command
  ","    'my/org-bookmark
  "ESC"  'keyboard-quit
  "U"    'kmacro-start-macro
  )

(general-create-definer my-local-leader-def
  :prefix "mm"
  )

(general-create-definer my-s-def
  :prefix "s")
(my-s-def
  :states '(normal motion visual) 
  :keymaps 'override
  ","    'tmux-ls
  "'"    'tmux-display-pane-numbers
  ";"    'hydra-tmux-command-history2/body
  "-"    'tmux-last-dir
  "_"    'tmux-split-window-vertical
  "b"    'tmux-capture-pane
  "c"    'tmux-ctrl-c
  "d"    'tmux-ctrl-d
  "e"    'tmux-clear-pane
  "f"    'tmux-minibuffer-run-shell-cmd
  "h"    'tmux-home-dir
  "i"    'tmux-insert-state
  "j"    'tmux-ivy-run-shell
  "k"    'hydra-tmux-command-history/body
  "l"    'tmux-swap-pane
  "m"    'tmux-toggle-zoom
  "n"    'tmux-send-no
  "p"    'tmux-pwd
  "q"    'tmux-quit
  "r"    'tmux-rename-window
  "s"    'tmux-send-region
  "t"    'hydra-tmux-window-config/body
  "u"    'tmux-up-dir
  "v"    'tmux-terminal-view
  "w"    'tmux-last-window
  "y"    'tmux-send-yes
  "z"    'tmux-ctrl-z
  "D"    'tmux-kill-pane
  "N"    'tmux-new-window
  "SPC"  'hydra-tmux-copy-mode/body
  "RET"  'tmux-ctrl-m
  "0"     'tmux-select-window-0
  "1"     'tmux-select-window-1
  "2"     'tmux-select-window-2
  "3"     'tmux-select-window-3
  "4"     'tmux-select-window-4
  "5"     'tmux-select-window-5
  "6"     'tmux-select-window-6
  "7"     'tmux-select-window-7
  "8"     'tmux-select-window-8
  "9"     'tmux-select-window-9

  ;; "h"    'tmux-history-cmd
  ;; "j"    'tmux-goto-active-pane
  ;; "n"    'tmux-rename-window
  ;; "t"    'tmux-terminal-view
  ;; "_"    'tmux-move-pane-vertical
  ;; "|"    'tmux-move-pane-horizantal
  ;; ","    'tmux-ls
  )

;;;; enable escape key
(defun my/keyboard-quit ()
  (interactive)
  (if (evil-ex-hl-active-p 'evil-ex-search)
      (evil-ex-delete-hl 'evil-ex-search))
  (keyboard-quit))

(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key evil-normal-state-map [escape] 'my/keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

(with-eval-after-load "helm"
  (define-key helm-map  [escape] 'keyboard-escape-quit))
(with-eval-after-load "ivy"
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))
(with-eval-after-load "popup"
  (define-key popup-menu-keymap  [escape] 'keyboard-quit))

;;;;; escape key for terminal mode 
(defvar my-esc-map nil)

(defun my-esc (map)
  (if (and (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for 0.1))
      [escape]
    map))

(defun my-init-esc ()
  (let ((term (frame-terminal)))
    (when (and (equal (terminal-live-p term) 't)
               (not (terminal-parameter term 'my-esc-map)))
      (let ((my-esc-map (lookup-key input-decode-map [?\e])))
        (set-terminal-parameter term 'my-esc-map my-esc-map)
        (define-key input-decode-map [?\e]
          `(menu-item "" ,my-esc-map :filter ,#'my-esc))))))

(add-hook 'minibuffer-setup-hook 'my-init-esc)

(add-hook 'compilation-mode-hook
          (lambda ()
            (local-unset-key "g")
            (define-key evil-normal-state-local-map "j" 'compilation-next-error)
            (define-key evil-normal-state-local-map "k" 'compilation-previous-error)
            ))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "s-j") 'company-select-next)
  (define-key company-active-map (kbd "s-k") 'company-select-previous)
  )

(add-hook 'Custom-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'Custom-newline)
            ))

(add-hook 'diff-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map "j" 'diff-hunk-next)
            (define-key evil-normal-state-local-map "k" 'diff-hunk-prev)
            (define-key evil-normal-state-local-map "r" 'vc-refresh-state)
            (define-key evil-normal-state-local-map (kbd "RET") 'diff-goto-source)
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key   "a" 'dired-copy-filename-as-kill)
            (local-unset-key "b")
            (local-set-key   "c" 'tmux-sync-location-with-emacs)
            (local-set-key   "d" 'my/dired-up-directory)
            (local-unset-key "e")
            (local-unset-key "h")
            (local-set-key   "i" 'wdired-change-to-wdired-mode)
            (define-key evil-normal-state-local-map   "j" 'dired-next-line)
            (define-key evil-normal-state-local-map   "k" 'dired-previous-line)
            (local-unset-key "l")
            (local-unset-key "n")
            ;;(local-set-key "o" 'dired-find-file)
            (local-unset-key "p")
            (local-unset-key "q")
            (define-key evil-normal-state-local-map   "r" 'revert-buffer)
            (local-unset-key "t")
            (local-unset-key "v")
            (local-unset-key "w")
            (local-unset-key "y")
            (local-set-key "L" 'tmux-tail-this-file)
            (local-set-key "E" 'dired-toggle-read-only)
            ;;(local-set-key "O" 'dired-find-file-other-window)
            (local-set-key "T" 'dired-toggle-marks)
            (local-set-key "X" 'tmux-dired-run-file)
            (local-unset-key ",")
            (local-unset-key "'")
            (local-unset-key ".")
            (local-unset-key (kbd "DEL"))
            ;; (define-key evil-normal-state-local-map   ";" 'avy-goto-word-or-subword-1)
            (define-key evil-normal-state-local-map  (kbd "<return>") 'dired-find-file) 
            (local-set-key   "x" 'dired-mark)
            ))

(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (local-set-key [escape] 'ediff-quit)
            ;; (define-key ediff-mode-map "f" nil)
            ;; (define-key ediff-mode-map "g" nil)
            ;; (define-key ediff-mode-map "m" nil)
            ))

(define-key ivy-minibuffer-map (kbd "M-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "M-j") 'next-line)
(define-key ivy-minibuffer-map (kbd "M-k") 'previous-line)
;; keybindings on mac side
(define-key ivy-minibuffer-map (kbd "s-o") 'ivy-occur)
(define-key ivy-minibuffer-map (kbd "s-j") 'next-line)
(define-key ivy-minibuffer-map (kbd "s-k") 'previous-line)
(define-key ivy-minibuffer-map (kbd "M-;") 'kill-ivy-file)

(define-key org-mode-map (kbd "M-h") 'outline-up-heading)
(define-key org-mode-map (kbd "M-j") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "M-k") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "M-l") 'org-goto-first-child)
(define-key org-mode-map (kbd "M-m") '(lambda () (interactive) (outline-back-to-heading)))
(define-key org-mode-map (kbd "M-RET") 'org-meta-return)

(define-key org-mode-map (kbd "<s-right>") 'org-metaright)
(define-key org-mode-map (kbd "<s-left>") 'org-metaleft)
(define-key org-mode-map (kbd "<s-up>") 'org-metaup)
(define-key org-mode-map (kbd "<s-down>") 'org-metadown)
(define-key org-mode-map (kbd "<S-s-right>") 'org-shiftmetaright)
(define-key org-mode-map (kbd "<S-s-left>") 'org-shiftmetarleft)
(define-key org-mode-map (kbd "<S-s-up>") 'org-metaup)
(define-key org-mode-map (kbd "<S-s-down>") 'org-metadown)
(define-key org-mode-map (kbd "<s-return>") 'org-meta-return)

(defun kill-ivy-file (x)
  (interactive)
  (kill-buffer x)
  (ivy--reset-state ivy-last)
    )

(add-hook 'ivy-occur-grep-mode-hook
          (lambda ()
            (local-unset-key "f")
            (local-unset-key "g")
            (local-unset-key "n")
            (local-unset-key "q")
            (local-unset-key "w")
            (local-set-key "o" 'ivy-occur-press)
            (local-set-key "r" 'ivy-occur-revert-buffer)
            (local-set-key ";" 'evil-avy-goto-subword-1)
            (local-set-key (kbd "RET") 'ivy-occur-press-and-switch)
            ))

(add-hook 'magit-status-mode-hook
          (lambda ()
            (local-unset-key "i")))

(add-hook 'Info-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'Info-follow-nearest-node)
            (define-key evil-normal-state-local-map "a" 'Info-history-back)
            (define-key evil-normal-state-local-map "i" 'Info-next)
            (define-key evil-normal-state-local-map "o" 'Info-prev)
            (define-key evil-normal-state-local-map "u" 'Info-up)
            ))

(add-hook 'tablist-mode-map-hook
          (lambda ()
            (define-key evil-normal-state-local-map "T" 'tablist-toggle-marks)
            (define-key evil-normal-state-local-map "." 'tablist-mark-forward)
            (define-key evil-normal-state-local-map "<" 'tablist-shrink-column)
            (define-key evil-normal-state-local-map ">" 'tablist-enlarge-column)
            (define-key evil-normal-state-local-map "s" 'tablist-sort)
            (define-key evil-normal-state-local-map "u" 'tablist-unmark-forward)
            ))

(add-hook 'rg-mode-hook
          (lambda ()
            (local-unset-key "f")
            (local-unset-key "g")
            ))

(add-hook 'ripgrep-search-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'compile-goto-error)))

(add-hook 'shell-mode-hook
          (lambda ()
            ;(define-key evil-normal-state-local-map [escape] 'my/comint-interrupt-subjob)
            (define-key shell-mode-map [escape] nil)
            ))

(defhydra hydra-buffer (:color blue)
  "buffer commands"
  ("c" recover-this-file "recover this file")
  ("d" diff-buffer-with-file "diff")
  ("e" ediff-buffers "ediff")
  ("i" ibuffer  "ibuffer")
  ("k" kill-other-buffers "kill others (unmodifed)")
  ("m" mark-whole-buffer "mark whole buffer")
  ("n" rename-buffer "rename")
  ("s" revert-buffer-with-coding-system "change end of line coding")
  ("v" revert-buffer "revert")
  )

(defhydra hydra-recentf (:color blue)
  ("c" my/counsel-recentf-c "c++")
  ("d" my/counsel-recentf-dockerfile "dockerfile")
  ("e" my/counsel-recentf-el "el")
  ("f" projectile-recentf)
  ("j" my/counsel-recentf-json "json")
  ("k" my/counsel-recentf-cmake "cmake")
  ("l" my/counsel-recentf-log "log")
  ("m" my/counsel-recentf-misc "misc")
  ("o" my/counsel-recentf-org "org")
  ("p" my/counsel-recentf-py "python")
  ("q" my/counsel-recentf-sql-cql "[sc]ql")
  ("s" my/counsel-recentf-sh "sh")
  ("t" my/counsel-recentf-txt "txt")
  ("x" my/counsel-recentf-xml "xml")
  ("r" counsel-recentf "recent")
  ("," my/counsel-recentf-set-file-extension "set designated")
  ("." my/counsel-recentf-designated "designated")
  )

(defhydra hydra-projectile-recentf (:color blue)
  ("c" my/counsel-projectile-recentf-c "c++")
  )

(defhydra hydra-help (:color blue)
  ("a" counsel-apropos "apropos")
  ("f" counsel-describe-function "function")
  ("k" describe-key "key binding")
  ("v" counsel-describe-variable "variable")
  )

(defhydra hydra-kmacro-end-or-call-macro-repeat (:body-pre (kmacro-end-or-call-macro-repeat 1))
  ("u" kmacro-end-or-call-macro-repeat))

(defhydra hydra-misc (:color blue)
  ("a"  my-clear "comint clear buffer")
  ("b"  my/bookmark-set "set bookmark")
  ("c"  compile "compile")
  ("d"  comment-dwim "comment dwim")
  ("g"  grep "grep")
  ("l"  org-store-link "org link")
  ("k"  kill "kill ps")
  ("o"  org-capture "org capture")
  ("p"  display-ps "display ps")
  ("r"  recompile "recompile")
  ("s"  hydra-shell-cmd/body "shell cmd")
  ("t"  display-top "display top")
  ("v"  hydra-vc/body)
  ("x"  docker-init "docker init")
  ("C"  my/compilation-command "custom compile")
  ("T"  run-UT-case "run UT")
  )

(defhydra hydra-org-find (:color blue)
  ("f" my/find-org-file "file search")
  ("o" my/counsel-rg-org-search "heading search") 
  )

(defhydra hydra-shell-cmd (:color blue :hint nil)
  "
  _m_: man   _p_: ps   _t_: top
"
  ("m" display-manual)
  ("p" ps)
  ("t" top :exit nil)
  )
 
(defhydra hydra-tabbar-navigation ()
  ("a" my/tb-add-to-group "add" :exit t)
  ("c" my/tb-change-group-description "description")
  ("j" tabbar-backward-tab "left")
  ("k" tabbar-forward-tab "rifht")
  ("b" tabbar-move-current-tab-one-place-left "move left")
  ("f" tabbar-move-current-tab-one-place-right "move right")
  ("s" my/tb-jump-to-group "jump")
  ("t" move-current-tab-to-top "top")
  ("x" my/tb-delete-from-group "delete")
  ("SPC" evil-scroll-down)
  ("DEL" evil-scroll-up)
  ("ESC" nil :exit t)
  )

(defhydra hydra-tmux-command-history (:body-pre (tmux-command-history-prev) :hint nil)
  "
  _k_: prev _j_: next _c_: cancel _e_: clear _RET_: run
"
  ("c" tmux-ctrl-c :exit t)
  ("e" tmux-clear-pane)
  ("j" tmux-command-history-next)
  ("k" tmux-command-history-prev)
  ("RET" tmux-ctrl-m :exit t)
  )

(defhydra hydra-tmux-command-history2 (:body-pre (tmux-begin-cmd-history) :hint nil :foreign-keys warn)
  "
  ;; _DEL_: page up _SPC_: page down _j_: down _k_: up _RET_: select  
"
  ("DEL" tmux-page-up)
  ("SPC" tmux-page-down)
  ("RET" tmux-run-a-history-cmd :exit t)
  ("j" tmux-copy-mode-down)
  ("k" tmux-copy-mode-up)
  ("<escape>" tmux-quit-copy-mode :exit t)
  )

(defhydra hydra-tmux-copy-mode (:body-pre (tmux-begin-copy-mode) :hint nil :foreign-keys warn)
  "
  ;; _DEL_: page up _SPC_: page down _j_: down _k_: up 
"
  ("DEL" tmux-halfpage-up)
  ("SPC" tmux-halfpage-down)
  ("j" tmux-copy-mode-down)
  ("k" tmux-copy-mode-up)
  ("<escape>" tmux-quit :exit t)
  )

(defhydra hydra-tmux-window-config (:body-pre (setq tmux-selected-pane "0"))
  ("h" tmux-resize-pane-left)
  ("l" tmux-resize-pane-right)
  ("j" tmux-resize-pane-down)
  ("k" tmux-resize-pane-up)
  ("s" tmux-select-pane)
  ("-"  tmux-split-window-vertical)
  ("|" tmux-split-window-horizontal)
  )
   
(defhydra hydra-toggle (:color blue)
  "toggle"
  ("a"   auto-revert-tail-mode  "auto revert")         
  ("b"   tabbar-mode "tabbar")
  ("c"   toggle-buffer-coding-system "coding system")
  ("d"   display-line-numbers-mode "line number")
  ("m"   toggle-view-tmux "view tmux")
  ("o"   origami-mode "origomi")
  ("p"   rainbow-delimiters-mode  "rainbow delimiters")
  ("r"   read-only-mode   "read only")
  ("s"   visible-mode      "visible")
  ("t"   toggle-truncate-lines "truncate lines")
  ("v"   visual-line-mode  "visual line")
  ("w"   whitespace-mode  "whitespace")
  ) 

(defhydra hydra-vc (:color blue)
  ("=" vc-diff "diff")
  ("e" vc-ediff "ediff")
  ("g" vc-annotate "annotate")
  ("l" vc-print-log "print log")
  ("u" vc-revert "revert")
  ("d" vc-root-diff "root diff")
  )

(defhydra hydra-window-left (:body-pre (move-splitter-left))
  ("h" move-splitter-left)
  ("l" move-splitter-right)
  )
   
(defhydra hydra-window-down (:body-pre (move-splitter-down))
  ("j" move-splitter-down)
  ("k" move-splitter-up)
  )
   
(defhydra hydra-window-up (:body-pre (move-splitter-up))
  ("j" move-splitter-down)
  ("k" move-splitter-up)
  )
   
(defhydra hydra-window-right (:body-pre (move-splitter-right))
  ("h" move-splitter-left)
  ("l" move-splitter-right)
  )
   
(provide 'init-keybindings)
