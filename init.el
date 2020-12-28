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

;; Installing all the listed packages
(eval-when-compile
  (dolist (package '(use-package diminish bind-key))
    (unless (package-installed-p package)
      (package-install package))
    (require package)))

;; (setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(load-theme 'nord t)

;; Evil mode
;;(require 'evil)
;;(evil-mode 1)

(use-package which-key
  :ensure t)

;; Magit git interface
(use-package magit
  :ensure t)

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
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :init)

;; Integrate projectile with treemacs
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; Flycheck for coding automplete
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide 'init)
;;; init.el ends here
