;;; init-packages

;; errors getting archives in Debian 10 for some emacs 26.x errors.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; setting up the MELPA and other repos
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-unstable" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; set repository priority, favoring stable
(setq package-archive-priorities
      '(("melpa" . 50)
        ("gnu" . 10)
        ("org" . 10)
        ("melpa-unstable" . 1)))

;; install use-package if not installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package-ensure)
(setq
 ;; ensure use-package by default
 use-package-always-ensure t
 ;; use newest version of packages
 load-prefer-newer t)

;; compile packages
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(provide 'init-packages)
;;; init-packages.el ends here
