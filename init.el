;;; init --- Summary

;;; Commentary:
;; see README.org

;; add elisp directory to load path for my config modules
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'init-personal)
(require 'init-packages)
(require 'init-functions)
(require 'init-general)
(require 'init-keys)
(require 'init-interaction)
(require 'init-hydra)
(require 'init-interface)
(require 'init-org)
(require 'init-tramp)
(require 'init-vterm)
;; packages which operate independently and don't have enough configuration to merit their own file
(require 'init-utilities)
(require 'init-exwm)

(provide 'init)
;;; init.el ends here
