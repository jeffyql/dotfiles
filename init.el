(setq package-native-compile t)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/mib/scripts/emacs")

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; (add-to-list 'package-archives
;;              '("org"   . "http://orgmode.org/elpa/"))
;; (use-package org
;;   :ensure org-plus-contrib
;;   :pin org)

(require 'init-local-info)
(require 'init-keybindings)
(require 'init-core)
(require 'init-packages)
(require 'init-recentf)
(require 'init-vterm)

(require 'init-capture)
(require 'init-narrow)
(require 'init-saved-lists)
(require 'init-send-command)
(require 'init-lib)
(require 'init-elisp)

(require 'init-ui)
;; (require 'cogent-modeline)

(require 'init-org)
(require 'init-org-roam)
(require 'init-org-keybinding)
(require 'init-remote)
(require 'init-shell)
(require 'init-snippets)
;; (require 'init-ide)
(require 'init-terminal)

(require 'init-prog)

(require 'init-eglot)
(require 'init-hydra)




;(require 'init-dired)
;(require 'init-c++)
;; (require 'init-tramp)
; (require 'init-magit)
;; (require 'init-docker)
;; (require 'init-postgres)

;;(server-start)
(require 'init-python)
(require 'init-julia)
(provide 'init)

