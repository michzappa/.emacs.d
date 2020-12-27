;; Heavily "inspired" by hlissner's DOOM distribution

(setq user-emacs-directory "~/my-emacs.d")
;; (setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(require 'package)
(add-to-list `package-archives `("melpa" . "http://melpa.org/packages/"))
(package-initialize)

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

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
