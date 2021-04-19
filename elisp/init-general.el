;; init-general

(setq
 ;; scroll compilation output in the window
 compilation-scroll-output t
 ;; reduce the frequency of garbage collection by making it happen on
 ;; each 50MB of allocated data (the default is on every 0.76MB)
 gc-cons-threshold 50000000
 ;; warn when opening files bigger than 100MB
 large-file-warning-threshold 100000000
 ;; quit Emacs directly even if there are running processes
 confirm-kill-processes nil
 ;; quit Emacs directly even if there are running processes
 confirm-kill-processes nil
 ;; don't overwrite outside program item on clipboard when switching to emacs
 save-interprogram-paste-before-kill t
 ;; don't overwrite outside program item on clipboard when switching to emacs
 save-interprogram-paste-before-kill t
 ;; add newline at end of files by default, required by some modes
 mode-require-final-newline 'visit-save
 ;; limit number of items recentf stores
 recentf-max-menu-items 25
 recentf-max-saved-items 25
 ;; store all backup and autosave files in the tmp dir
 backup-directory-alist
 `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t)))

;; pretty symbols like lambda, and, and or in certain modes
(global-prettify-symbols-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; reload files from disk
(global-auto-revert-mode)

;; recent file store
(recentf-mode 1)

;; Add color formatting to *compilation* buffer
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

;; mouse config
(setq-default  scroll-margin 0
               scroll-step 1
               mouse-wheel-progressive-speed nil
               scroll-conservatively 100000
               scroll-preserve-screen-position 1)

;; highlight the current line
(global-hl-line-mode +1)

;; "colors" hex codes or color words
(use-package rainbow-mode
  :hook
  (emacs-lisp-mode . rainbow-mode))

;; insert pairs of delimiters
(electric-pair-mode)
;; prevent <> when trying to make a src block in org mode
(add-hook 'org-mode-hook
          (lambda () (setq-local electric-pair-inhibit-predicate
                            (lambda (c)
                              (if (eq c ?\<)
                                  t
                                (electric-pair-inhibit-predicate c))))))

;; enhanced paren management, currently just using to highlight the match of the paren under the point
(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode))

;; colors matching delimiters
(use-package rainbow-delimiters
  :hook
  ((prog-mode) . rainbow-delimiters-mode))

;; assumes default format tool based off major mode
(use-package format-all)

;; major mode for markdown files
(use-package markdown-mode)

;; wraps visual lines
(global-visual-line-mode)

;; basic file settings
(setq-default
 ;; newline at end of file
 require-final-newline t
 ;; wrap lines at 80 characters
 fill-column 100)

;; delete trailing whitespace when saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-general)
;; init-general.el ends here
