(general-create-definer my-f-def
  :prefix "f")

(general-create-definer my-g-def
  :prefix "g")

(general-create-definer my-m-def
  :prefix "m")

(general-def '(motion normal visual) 'override
  ";"   'avy-goto-word-1
  ","   'my/select-tab-or-toggle-buffer
  "'"   'my/toggle-buffer
  "t"   'universal-argument
  "s"   'my/select-window
 )

(general-def '(motion normal visual)
  "q"   'my/kill-this-buffer
  "DEL" 'my/evil-scroll-up
  "SPC" 'my/evil-scroll-down
  "TAB" nil
 )

(general-def 'visual
 "a"  'mark-whole-buffer
 "i"  'evil-visual-line
 "o"  'evil-visual-block
 "r"  'evil-surround-region
 "u"  'er/contract-region
 "v"  'er/expand-region
 )

(define-key universal-argument-map "t" 'universal-argument-more)

(general-def
 "M-i" 'completion-at-point
 "M-." 'xref-find-definitions
 "M-?" 'xref-find-references
 "<s-right>" nil
 "<s-left>" nil
 "<s-up>" nil
 "<s-down>" nil
 )

(my-f-def 'normal 'override
  "ESC"  'keyboard-quit
  "a"    'my/recentf-misc
  "d"    'dired
  "e"    'my/recentf-el
  "f"    'counsel-find-file
  "g"    'grep
  "h"    'hydra-help/body
  "i"    'counsel-imenu
  "j"    'my/recentf-main-language-by-project
  "l"    'my/counsel-rg
  "k"    'my/counsel-rg-at-point
  "m"    'counsel-grep-or-swiper
  "n"    'my/swiper-symbol
  "o"    'my/recentf-org
  "p"    'my/projectile-find-file
  "r"    'my/counsel-rg-org-search
  "s"    'ivy-switch-buffer
  "t"    'my/ripgrep-this-file
  "u"    'counsel-bookmark
  "v"    'my/counsel-narrowed-indirect
  "w"    'tmux-select-active-window
  "y"    'counsel-yank-pop
  ","    'my/swiper-current-kill
  ";"    'my/counsel-rg-current-kill
  "."    'ivy-resume
  "7"    'select-vterm-7
  "8"    'select-vterm-8
  "9"    'select-vterm-9
  "0"    'select-vterm-0
  )

(my-g-def '(normal motion visual) 'override
  "ESC"  'keyboard-quit
  "a"    'evil-first-non-blank
  "b"    (lambda () (interactive) (move-to-window-line -1))
  "c"    'my/last-log-file
  "d"    'dired-jump
  "e"    'evil-last-non-blank
  "f"    'firefox-to-front
  "g"    'evil-goto-first-line
  "h"    (lambda () (interactive) (dired "~"))
  "i"    'my/goto-indirect-narrow-at-point
  "j"    'my/open-last-main-language-file
  "k"    'hydra-last-buffer-by-mode/body
  "l"    'evil-goto-line
  "m"    'my/open-last-misc-file
  "n"    'select-vterm
  "o"    'my/open-last-org-file
  "p"    'my/projectile-dired
  "s"    '(lambda () (interactive) (switch-to-buffer "*scratch*"))
  "t"    (lambda () (interactive) (move-to-window-line 0))
  ";"    'goto-last-change
  ","    'pop-to-mark-command
  "."    'evil-scroll-line-to-center
  "G"    (lambda () (interactive) (find-file my-org-checklist-file))
  "SPC"  'switch-to-terminal
  "RET"  'hydra-org-cycle-agenda-files/body
  )

(my-m-def 'normal 'override
  "a"    'my/kill-ring-save-symbol-at-point
  "b"    'hydra-buffer/body
  "c"    nil    ;;c key is reserved as major mode leader 
  "d"    nil
  "e"    'my/send-command
  "f"    nil
  "g"    nil
  "i"    'my/switch-indirect-narrow
  "h"    'hydra-move-window-splitter-left-or-up/body
  "j"    'hydra-scroll-line-up/body
  "k"    'hydra-scroll-line-down/body
  "l"    'hydra-move-window-splitter-right-or-down/body
  "m"    'my/save-buffer
  "n"    'my/evil-ex-search-word-forward
  "o"    'occur
  "p"    'my/projectile-select-project
  "q"    'my/delete-or-split-window
  "r"    'my/open-next-line
  "s"    'my/occur-at-point
  "t"    'hydra-toggle/body
  "u"    'hydra-kmacro-end-or-call-macro-repeat/body
  "w"    'my/swap-window
  "x"    'counsel-M-x
  "v"    nil
  "z"    'my/toggle-or-split-window
  "B"    'balance-windows
  "D"    'save-buffers-kill-emacs
  "G"    'magit-dispatch-popup
  "J"    'move-line-down
  "K"    'move-line-up
  "T"    'test
  "X"    'shell-command
  ","    'my/org-store-link-to-current-line
  "ESC"  'keyboard-quit
  "SPC"  'show-terminal
  "U"    'kmacro-start-macro
  "1"    'vterm-by-number
  "2"    'vterm-by-number
  "3"    'vterm-by-number
  "4"    'vterm-by-number
  "5"    'vterm-by-number
  "6"    'vterm-by-number
  )

(general-create-definer my-mc-def
  :prefix "mc"
  )

(general-create-definer my-md-def
  :prefix "md"
  )

(my-md-def '(normal motion visual) 'override
  "e"  'hydra-edit/body
  "f"  'hydra-file/body
  "v"  'hydra-misc/body
  )

(general-create-definer my-mf-def
  :prefix "mf"
  )

(my-mf-def '(normal motion visual) 'override
  "c"   'org-roam-db-build-cache
  "d"   'org-roam
  "f"   'org-roam-find-file
  "i"   'org-roam-insert
  "s"   'my/org-store-link-to-current-line
  "t"   'my/org-roam-new-tab
  )

(general-create-definer my-mv-def
  :prefix "mv"
  )

;; global misc commands
(my-mv-def
  :states '(normal motion visual)
  :keymaps 'override
  "b"   'my/buffer-to-file-or-new-buffer
  "f"   'magit-find-file
  "g"   'my/magit-status
  "h"   'highlight-symbol-at-point-all-windows
  "k"   'my/current-kill-to-file-or-new-buffer
  "p"   'my/add-or-delete-command-snippet
  "s"  (lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively 'deadgrep)))
  "t"   'tab-new
  "x"   'etcc-on
  )

(general-create-definer my-z-def
  :prefix "z")

(my-z-def '(normal motion visual) 'override
  "h"   'evil-scroll-left
  "l"   'evil-scroll-right
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

;; (with-eval-after-load "helm"
;;   (define-key helm-map  [escape] 'keyboard-escape-quit))
(with-eval-after-load "ivy"
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))
;; (with-eval-after-load "popup"
;;   (define-key popup-menu-keymap  [escape] 'keyboard-quit))

(add-hook 'compilation-mode-hook
          (lambda ()
            (local-unset-key "g")
            (define-key evil-normal-state-local-map (kbd "a") 'first-error)
            (define-key evil-normal-state-local-map (kbd "d") 'next-error)
            (define-key evil-normal-state-local-map (kbd "u") 'previous-error)
            (define-key evil-normal-state-local-map (kbd "M-j") 'compilation-next-error)
            (define-key evil-normal-state-local-map (kbd "M-k") 'compilation-previous-error)
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
            (local-set-key   "c" 'goto-vterm)
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
            (local-unset-key (kbd "SPC"))
            ;; (define-key evil-normal-state-local-map   ";" 'avy-goto-word-or-subword-1)
            (define-key evil-normal-state-local-map  (kbd "<return>") 'dired-find-alternate-file)
            (local-set-key   "x" 'dired-mark)
            ))

(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (local-set-key [escape] 'ediff-quit)
            ;; (define-key ediff-mode-map "f" nil)
            ;; (define-key ediff-mode-map "g" nil)
            ;; (define-key ediff-mode-map "m" nil)
            (define-key evil-motion-state-local-map "q" 'ediff-quit)
            (define-key evil-motion-state-local-map "," 'my/select-window-or-tab)
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
(define-key org-mode-map (kbd "M-l") 'org-goto-first-child-cmd)
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

(add-hook 'ivy-occur-mode-hook
          (lambda ()
            (local-unset-key "f")
            (local-unset-key "g")
            (local-unset-key "n")
            (local-unset-key "q")
            (local-unset-key "w")
            (local-set-key "o" 'ivy-occur-press)
            (local-set-key "r" 'ivy-occur-revert-buffer)
            (local-set-key ";" 'evil-avy-goto-subword-1)
            (define-key evil-normal-state-local-map (kbd "RET") 'ivy-occur-press-and-switch)
            ))

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

(add-hook 'xref--xref-buffer-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "RET") 'xref-goto-xref)))

(defhydra hydra-buffer (:color blue)
  "buffer commands"
  ("a" new-scratch-buffer-new-window "scratch buffer")
  ("c" recover-this-file "recover this file")
  ("d" diff-buffer-with-file "diff")
  ("e" ediff-buffers "ediff")
  ("i" ibuffer  "ibuffer")
  ("k" kill-other-buffers "kill others (unmodifed)")
  ("n" rename-buffer "rename")
  ("s" revert-buffer-with-coding-system "change end of line coding")
  ("v" revert-buffer "revert")
  )

(defhydra hydra-edit ()
  ("a" align "align" :exit t)
  ("j" move-line-down "line down")
  ("k" move-line-up "line up")
  ("r" align-regexp "align regexp")
  ("t" delete-trailing-whitespace "delete trailing WS" :exit t)
  ("y"  evil-commentary-yank "commentary yank" :exit t)
  ("SPC" just-one-space "one space" :exit t)
  )

(defhydra hydra-file (:color blue)
  ("d"  ediff-current-file "ediff")
  ("e"  make-empty-file "make empty file")
  ("f"  write-file "write file")
  ("l"  my/save-capture "capture")
  ("o"  org-capture "org capture")
  )

(defhydra hydra-help (:color blue)
  ("a" counsel-apropos "apropos")
  ("f" counsel-describe-function "function")
  ("i" info)
  ("k" describe-key "key binding")
  ("m" describe-mode "mode")
  ("v" counsel-describe-variable "variable")
  )

(defhydra hydra-misc (:color blue)
  ("a"  symbol-overlay-put "symbol-overlay-put")
  ("b"  my/buffer-to-file "capture buffer")
  ("c"  my/current-kill-to-file "capture current kill")
  ("d"  my/ffap "ffap")
  ("f"  get-buffer-file-name "file name")
  ("F"  get-buffer-file-path "file path")
  ("g"  grep "grep")
  ("l"  org-store-link "org link")
  ("k"  kill "kill ps")
  ("n"  tab-rename "tab new")
  ("m"  man "man")
  ("r"  recompile "recompile")
  ("q"  query-replace "query-replace")
  ("v"  hydra-vc/body "vc")
  ("w"  split-window-below "split window")
  )

(defhydra hydra-recentf (:color blue)
  ("c" my/counsel-recentf-c "c++")
  ("e" my/counsel-recentf-el "el")
  ("j" projectile-recentf)
  ("l" my/counsel-recentf-log "log")
  ("m" my/counsel-recentf-misc "misc")
  ("n" my/counsel-recentf-json "json")
  ("p" my/counsel-recentf-py "python")
  ("q" my/counsel-recentf-sql-cql "[sc]ql")
  ("s" my/counsel-recentf-sh "sh")
  ("t" my/counsel-recentf-txt "txt")
  ("v" my/counsel-recentf-java "java")
  ("x" my/counsel-recentf-xml "xml")
  ("r" counsel-recentf "recent")
  ("," my/counsel-recentf-set-file-extension "set designated")
  ("." my/counsel-recentf-designated "designated")
  )

(defhydra hydra-last-buffer-by-mode (:color blue)
  ("n" my/open-last-narrowed-buffer "narrowed")
  ("r" my/last-deadgrep-buffer "deadgrep")
  ("s" my/last-sql-buffer "sql")
  )

(defhydra hydra-scroll-line-down (:body-pre (evil-scroll-line-down 1))
  ("j" evil-scroll-line-up)
  ("k" evil-scroll-line-down)
  ("ESC" nil :exit t)
  )

(defhydra hydra-scroll-line-up (:body-pre (evil-scroll-line-up 1))
  ("j" evil-scroll-line-up)
  ("k" evil-scroll-line-down)
  ("ESC" nil :exit t)
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

(defhydra hydra-tmux-combo ()
  ("a"   tmux-ivy-run-shell)
  ("b"   tmux-capture-pane)
  ("c"   tmux-ctrl-c)
  ("d"   tmux-d)
  ("e"   tmux-clear-pane)
  ("g"   switch-to-terminal :exit t)
  ("h"   tmux-home-dir)
  ("i"   tmux-insert-state)
  ("j"   tmux-j)
  ("k"   tmux-k)
  ("l"   tmux-capture-compilation-output)
  ("m"   hydra-tmux-copy-mode/body)
  ("n"   tmux-n)
  ("o"   tmux-swap-pane)
  ("p"   tmux-pwd)
  ("q"   tmux-q)
  ("u"   tmux-up-dir)
  ("w"   tmux-last-window)
  ("y"   tmux-y)
  ("z"   tmux-z)
  ("D"   tmux-kill-pane)
  ("N"   tmux-new-window)
  ("RET" tmux-ctrl-m)
  ("SPC" tmux-space)
  ("-"   tmux-last-dir)
  ("."   tmux-cd-default-directory)
  (","   tmux-ls)
  ("_"   tmux-split-window-vertical)
  ("0"   tmux-select-window-0)
  ("1"   tmux-select-window-1)
  ("2"   tmux-select-window-2)
  ("3"   tmux-select-window-3)
  ("4"   tmux-select-window-4)
  ("5"   tmux-select-window-5)
  ("6"   tmux-select-window-6)
  ("7"   tmux-select-window-7)
  ("8"   tmux-select-window-8)
  ("9"   tmux-select-window-9)
)
(defhydra hydra-tmux-command-history (:body-pre (tmux-begin-cmd-history) :hint nil :foreign-keys warn)
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
  ;; _DEL_: page up _SPC_: page down _j_: down _k_: up _<escape>_: exit
"
  ("DEL" tmux-halfpage-up)
  ("SPC" tmux-halfpage-down)
  ("j" tmux-copy-mode-down)
  ("k" tmux-copy-mode-up)
  ("RET" tmux-capture-and-quit-copy-mode :exit t)
  ("<escape>" tmux-quit-copy-mode :exit t)
  ("q" tmux-quit-copy-mode :exit t)
  )

(defhydra hydra-tmux-repeat-command (:body-pre (tmux-command-history-prev) :hint nil)
  "
  _k_: prev _j_: next _b_: capture _c_: cancel _e_: clear _l_: pipe pane _m_: run _s_: show output _RET_: run and quit
"
  ("c" tmux-ctrl-c :exit t)
  ("b" tmux-capture :exit t)
  ("e" tmux-clear-pane)
  ("j" tmux-command-history-next)
  ("k" tmux-command-history-prev)
  ("l" tmux-pipe-pane)
  ("m" tmux-ctrl-m)
  ("s" tmux-show-pipe :exit t)
  ("RET" tmux-ctrl-m :exit t)
  ("<escape>" tmux-ctrl-c :exit t)
  )

(defhydra hydra-tmux-pipe-pane (:color blue)
  ("c" tmux-clean-pane-pipe "clean")
  ("f" tmux-fetch-pane-pipe "fetch")
  ("r" tmux-restart-pane-pipe "rstart")
  )

(defhydra hydra-tmux-window-config (:body-pre (setq tmux-selected-pane "0"))
  ("h" tmux-resize-pane-left)
  ("l" tmux-resize-pane-right)
  ("j" tmux-resize-pane-down)
  ("k" tmux-resize-pane-up)
  ("s" tmux-select-pane)
  ("-"  tmux-split-window-vertical)
  ("|" tmux-split-window-horizontal)
  ("<escape>" tmux-select-pane-0 :exit t)
  )


(defhydra hydra-toggle (:color blue)
  "toggle"
  ("a"   auto-revert-tail-mode  "auto revert")
  ("b"   tabbar-mode "tabbar")
  ("c"   compilation-mode "compilation mode")
  ("d"   toogle-display-buffer-alist "buffer-display")
  ("l"   display-line-numbers-mode "line number")
  ("g"   toggle-debug-on-error "debug on error")
  ("m"   toggle-view-tmux "view tmux")
  ("o"   origami-mode "origomi")
  ("p"   rainbow-delimiters-mode  "rainbow delimiters")
  ("r"   read-only-mode   "read only")
  ("s"   visible-mode      "visible")
  ("t"   toggle-truncate-lines "truncate lines")
  ("v"   visual-line-mode  "visual line")
  ("w"   whitespace-mode  "whitespace")
  ("C"   toggle-buffer-coding-system "coding system")
  )

(defhydra hydra-vc (:color blue)
  ("=" vc-diff "diff")
  ("e" vc-ediff "ediff")
  ("g" vc-annotate "annotate")
  ("l" vc-print-log "print log")
  ("u" vc-revert "revert")
  ("d" vc-root-diff "root diff")
  )

(defhydra hydra-move-window-splitter-left-or-up (:body-pre (my/move-splitter-left-or-up))
  ("h" my/move-splitter-left-or-up)
  ("l" my/move-splitter-right-or-down)
  )

(defhydra hydra-move-window-splitter-right-or-down (:body-pre (my/move-splitter-right-or-down))
  ("h" my/move-splitter-left-or-up)
  ("l" my/move-splitter-right-or-down)
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
