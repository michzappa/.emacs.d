;; init-utilities

;; RSS reader using an org-mode file for configuration
(use-package elfeed
  :bind ("C-c w" . elfeed)
  :init
  (use-package elfeed-org)
  :config
  (elfeed-org))

;;; Nov EPub Reader
;; EPub reader mode
(use-package nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook
  (nov-mode . visual-line-mode))

(require 'init-tex)

;;; Quelpa
;; a different wrapper for package.el that can also take packages from source
(use-package quelpa)

;; helper for using quelpa in the use-package macro
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;;; PDF-Tools
;; pdf enhancements
(use-package pdf-tools
  :init
  (pdf-tools-install)
  :hook
  (pdf-view-mode . pdf-view-midnight-minor-mode))

;; "smoothly" scroll through pdfs using multiple buffers
(use-package pdf-continuous-scroll-mode
  :defer t
  :quelpa (pdf-continuous-scroll-mode
           :fetcher git
           :url "https://github.com/dalanicolai/pdf-continuous-scroll-mode.el.git")
  :hook
  (pdf-view-mode . pdf-continuous-scroll-mode)
  :custom
  (pdf-view-have-image-mode-pixel-vscroll t))

;;; ERC
;; basic configuration for ERC
(setq
 erc-server "irc.freenode.net"
 erc-port 6667
 erc-nick "michzappa")
(provide 'init-utilities)
;; init-utilities.el ends here
