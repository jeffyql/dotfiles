(require 'package)
(setq package-enable-at-startup nil)
;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(if (string= system-type "darwin")
    (setq server-socket-dir (format "/tmp/emacs%d" (user-uid))))

(require 'init-core)
(require 'init-keybindings)
;;(require 'init-lib)
(require 'init-local-info)
(require 'init-elisp)
;(require 'init-packages)

(require 'init-ui)

(require 'init-org)
(require 'init-capture)
(require 'init-shell)
(require 'init-elisp)
(require 'init-snippets)
(require 'init-ide)
(require 'init-terminal)
(require 'init-vterm)

(require 'init-prog)






;(require 'init-dired)
;(require 'init-c++)
;; (require 'init-tramp)
; (require 'init-magit)
;; (require 'init-docker)
;; (require 'init-postgres)

;; (require 'config-macros)
;; (require 'config-python)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(server-start)
(provide 'init)

