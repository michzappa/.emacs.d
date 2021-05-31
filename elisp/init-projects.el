;; init-projects

;; keyboard-driven interface for git
(use-package magit
  :bind
  ("C-x g" . magit))

;; git gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode))

;; project manager
(use-package projectile
  :init
  (use-package ag)
  (use-package ibuffer-projectile)
  :custom
  (projectile-completion-system 'helm)
  (projectile-mode-line "Projectile")
  :config
  (projectile-mode +1))

(provide 'init-projects)
;; init-projects.el ends here
