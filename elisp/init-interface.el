;; init-interface

;; more icons
(use-package all-the-icons)

;; set my preferred zoom which keeps vterm opening vertically
(set-face-attribute 'default nil :height 141)

;; change default text scale, not just per-buffer
(use-package default-text-scale
  :config
  (default-text-scale-mode))

(setq
 ;; scratch screen
 inhibit-startup-screen t
 initial-scratch-message ""
 ;; turn off the hecking bell
 ring-bell-function 'ignore)

(setq-default
 ;; shallow tabs
 tab-width 2
 ;; <tab> inserts spaces by default
 indent-tabs-mode nil)

;; turn off things
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

;; initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; restrained themes designed for readability.
(use-package modus-themes
  :init
  (modus-themes-load-themes)
  :custom
  (modus-themes-intense-hl-line t)
  :config
  (modus-themes-load-vivendi)
  :bind
  ("C-c T" . modus-themes-toggle))

;; turns off all minor modes in modeline
(use-package minions
  :custom
  (minions-mode-line-lighter "")
  (minions-mode-line-delimiters '("" . ""))
  :config
  (minions-mode 1))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format '((:eval (projectile-project-name))))

;; line numbers, column number, size indication
(global-display-line-numbers-mode)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(provide 'init-interface)
;; init-interface.el ends here
