;; TODO
;; langs and flycheck
;; use purcell/emacs.d and DOOM as inspiration/sources
;; finally, organize and put in ORG file

(setq user-emacs-directory "~/my-emacs.d")

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

;; View recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

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
	 ("C-s" . swiper))
  :config (ivy-mode 1))

;; Sidebar file explorer
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
	("C-x p" . treemacs))
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'extended)
  (treemacs-follow-mode -1)
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
