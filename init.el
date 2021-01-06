;;; init --- the init.el for my emacs config
;;; Commentary:
;; TODO
;; maybe try eglot instead of lsp-mode?
;; more langs -- elixir?

;;; Code:
(setq user-emacs-directory "~/my-emacs.d/user-dir")

(setq user-full-name "Michael Zappa")
(setq user-mail-address "zapprich@gmail.com")

;; Setting up the MELPA repo
(require 'package)
(add-to-list `package-archives `("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Installing all the listed packages
(eval-when-compile
  (dolist (package '(use-package diminish bind-key))
    (unless (package-installed-p package)
      (package-install package))
    (require package)))

;; hide auto-revert-mode indication
(diminish auto-revert-mode)

;; minor mode for inserting parens, braces, quotes and the like in pairs
(electric-pair-mode)

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; Newline at end of file
(setq require-final-newline t)

;; Wrap lines at 80 characters
(setq-default fill-column 80)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; scratch screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; hecking bell
(setq ring-bell-function 'ignore)

;; turn off things
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
;; initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

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

;; line numbers, column number, size indication
(global-display-line-numbers-mode)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; change font size binding
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; keybinding to reload configuration
(global-set-key (kbd "C-c m") (lambda () (interactive) (load-file "~/my-emacs.d/init.el")))

(use-package diminish
  :ensure t)

;; Force Emacs to use shell path
(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize)

;; still working out what, if any, my custom keymap will be
  ;; (use-package crux
  ;;   :ensure t
  ;;   :config
  ;;   (global-set-key (kbd "C-k") #'crux-smart-kill-line)
  ;;   (global-set-key (kbd "C-s-RET") #'crux-smart-open-line-above)
  ;;   (global-set-key (kbd "s-RET") #'crux-smart-open-line))

;; highlight the current line
(use-package hl-line
  :ensure t
  :config
  (global-hl-line-mode +1))

(use-package windmove
  :ensure t
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings)
  ;; Make windmove work in Org mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

;; shows possible key combinations
(use-package which-key
  :ensure t
  :config
  (diminish 'which-key-mode)
  (which-key-mode))

;; magit git interface
(use-package magit
  :ensure t)

;; epub reader mode
(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; autocomplete interface for file search
(use-package counsel
  :ensure t
  :demand
  :bind 
   (("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file)
    ("C-x C-r" . counsel-recentf)
    ("C-s" . swiper))
  :commands ivy-mode)

(use-package smex
  :ensure t)

;; project manager
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/Projects"))
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; sidebar file explorer
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

;; integrate git with treemacs
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; integrate projectile with treemacs
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; company for text-completion
(use-package company
  :ensure t
  :config
  (diminish 'company-mode)
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

;; lsp-mode plus other recommended packages and configuration
(use-package lsp-mode
  :commands lsp
  :ensure t
  :config 
  (diminish lsp-mode))

(use-package lsp-ui
  :ensure t)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(setq lsp-completion-provider :capf)
(setq lsp-completion-enable t)

;; Help for elisp functions
(use-package eldoc
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package elixir-mode
  :ensure t
  :hook (elixir-mode . lsp))

;; hook up rust-mode with the language server
(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp))

;; cargo minor mode for cargo keybindings
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(add-hook 'c-mode-hook 'lsp)

(provide 'init)
;;; init.el ends here
