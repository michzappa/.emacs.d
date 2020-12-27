;; TODO
;; magit
;; lang and flycheck
;; show file names when searching
;; magit
;; use purcell/emacs.d and DOOM as inspiration/sources
;; finally, organize and put in ORG file

(setq user-emacs-directory "~/my-emacs.d")

(require 'package)
(add-to-list `package-archives `("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; (setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(require 'evil)
;;(evil-mode 1)

(load-theme 'nord t)

(require 'nov)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'which-key)
(which-key-mode)

(require 'magit)

