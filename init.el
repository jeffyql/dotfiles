(require 'package)
;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/mib/scripts/emacs")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'init-keybindings)
(require 'init-core)
(require 'init-packages)
(require 'init-recentf)


(require 'init-capture)
(require 'init-narrow)
(require 'init-saved-lists)
(require 'init-send-command)
(require 'init-lib)
(require 'init-local-info)
(require 'init-elisp)

(require 'init-ui)

(require 'init-org-keybinding)
(require 'init-org)
(require 'init-org-roam)
(require 'init-shell)
(require 'init-elisp)
(require 'init-snippets)
(require 'init-ide)
(require 'init-terminal)
(require 'init-vterm)

(require 'init-prog)

(require 'init-hydra)




;(require 'init-dired)
;(require 'init-c++)
;; (require 'init-tramp)
; (require 'init-magit)
;; (require 'init-docker)
;; (require 'init-postgres)

;; (require 'config-macros)
;; (require 'config-python)

;;(server-start)
(provide 'init)

