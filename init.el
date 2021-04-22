;;; init --- Summary

;;; Commentary:
;; see README.org

;; add elisp directory to load path for my config modules
(add-to-list 'load-path "~/.emacs.d/elisp")

(require 'init-personal)
(require 'init-packages)
;; additional functions
(require 'init-functions)
;; general settings
(require 'init-general)
;; keybindings not attached to a specific package
(require 'init-keys)
;; packages which add features to the way users interact with emacs
(require 'init-interaction)
(require 'init-hydra)
;; themes, other appearance configuration
(require 'init-interface)
;; config for packages dealing with project management
(require 'init-projects)
;; configuration for programming languages and the language server protocl
(require 'init-lang-lsp)
(require 'init-tex)
(require 'init-org)
(require 'init-tramp)
(require 'init-vterm)
;; packages which operate independently and don't have enough configuration to merit their own file
(require 'init-utilities)
(require 'init-exwm)

(provide 'init)
;;; init.el ends here
