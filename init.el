(require 'package)
(setq package-enable-at-startup nil)
;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(if (string= system-type "darwin")
    (setq server-socket-dir (format "/tmp/emacs%d" (user-uid))))

(require 'init-core)
(require 'init-keybindings)
(require 'init-lib)
(require 'init-local-info)
(require 'init-elisp)
(require 'init-packages)
(require 'init-ui)
(require 'init-org)
;(require 'init-dired)
(require 'init-shell)
;(require 'init-elisp)
;; (require 'init-tramp)
; (require 'init-magit)
;; (require 'init-work)
;; (require 'init-docker)
(require 'init-snippets)
(require 'init-ide)
;(require 'init-c++)
;; (require 'init-tabbar)
;; (require 'tb)
;; (require 'init-terminal)

;; (require 'init-postgres)

;; (require 'config-macros)
;; (require 'config-python)
(add-to-list 'load-path my-tmux-integration-dir)
;; (require 'tmux-head)

(server-start)
(provide 'init)

