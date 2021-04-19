;; init-interaction

;; jump around the screen by specifying 2 characters
(use-package avy
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-2))

;; company for text-completion
(use-package company
  :custom
  (company-idle-delay 0)
  (company-show-numbers t)
  :config
  (global-company-mode))

;; window-based completion and narrowing framework
(use-package helm
  :init (helm-mode t)
  :bind
  (("M-x"     . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini) ;; combines open buffers and recent files
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-r" . helm-recentf)
   ("M-i"   . helm-imenu)
   ("C-h a"   . helm-apropos)
   ("M-y" . helm-show-kill-ring)))

;; search and more, using helm as a backend
(use-package helm-swoop
  :bind
  ("C-s" . helm-swoop))

;; gives emacs info about your shell PATH
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; opens a new scratch buffer with the same mode as the current one
(use-package scratch
  :bind ("C-c s" . scratch))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; shows possible key combinations for multi-chord bindings.
(use-package which-key
  :config
  (which-key-mode))

;; navigate between windows using keyboard
(use-package windmove
  :ensure nil
  :bind
  (("M-S-<left>" . windmove-left)
   ("M-S-<right>" . windmove-right)
   ("M-S-<up>" . windmove-up)
   ("M-S-<down>" . windmove-down)

   ("C-M-<left>" . windmove-swap-states-left)
   ("C-M-<right>" . windmove-swap-states-right)
   ("C-M-<up>" . windmove-swap-states-up)
   ("C-M-<down>" . windmove-swap-states-down)))

(provide 'init-interaction)
;; init-interaction.el ends here
