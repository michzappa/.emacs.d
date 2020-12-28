;;; init --- Summary

;;; Commentary:
;; TODO
;; langs and flycheck
;; terminal?
;; finally, organize and put in ORG file

;;; Code:
;; user directory and reloading file
(setq user-emacs-directory "~/my-emacs.d")
(global-set-key (kbd "C-M-m") (lambda () (interactive) (load-file "~/my-emacs.d/init.el")))

;; Setting up the MELPA repo
(require 'package)
(add-to-list `package-archives `("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "Michael Zappa")
(setq user-mail-address "zapprich@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; Installing all the listed packages
(eval-when-compile
  (dolist (package '(use-package diminish bind-key))
    (unless (package-installed-p package)
      (package-install package))
    (require package)))

;; UI Stuff
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(load-theme 'nord t)
(blink-cursor-mode -1)
;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; line numbers
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Force Emacs to use shell path
(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

;; Keeps modeline tidy
(use-package diminish
  :ensure t)

;; Shows possible key combinations
(use-package which-key
  :ensure t)

;; Magit git interface
(use-package magit
  :ensure tg)

;; EPub reader mode
(use-package nov
  :ensure t)

;; Autocomplete interface
(use-package counsel
  :ensure t
  :demand
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf)
	 ("C-s" . swiper))
  :commands ivy-mode)

;; Sidebar file explorer
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
	("C-x p" . treemacs))
  :commands (treemacs-filewatch-mode
	     treemacs-git-mode
	     treemacs-follow-mode)
  :config
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))))

;; Integrate git with treemacs
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Project manager
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/Projects"))
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; Integrate projectile with treemacs
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; Flycheck for coding auto-complete
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Help for Elisp functions
(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(provide 'init)
;;; init.el ends here
