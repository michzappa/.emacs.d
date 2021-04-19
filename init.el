;;; init --- Summary

;;; Commentary:
;; see README.org

;;; Packages Setup
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

;;; Outline Setup
(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ("<tab>" . bicycle-cycle)
              ("<backtab>" . bicycle-cycle-global))
  :hook
  (prog-mode . outline-minor-mode)
  (prog-mode . hs-minor-mode))

(use-package outline-minor-faces
  :after outline
  :hook
  (outline-minor-mode-hook . outline-minor-faces-add-font-lock-keywords))

;;; Personal
;; telling emacs where to put stuff
(setq user-emacs-directory "~/.emacs.d/user-dir"
      custom-file          "~/.emacs.d/custom.el"
      bookmark-default-file "~/.emacs.d/bookmarks.el"
      abbrev-file-name     "~/.emacs.d/abbrev_defs"

      user-full-name       "Michael Zappa"
      user-mail-address    "zapprich@gmail.com")

;; add elisp directory to load path for additional elisp
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; Miscellanious
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

;; C-w to kill line, M-w to copy line
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

;; utility function for opening eshell in another window
(defun mz/eshell-other-window ()
  "Open 'eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

;; utility to reverse the result of fill-paragraph
(defun mz/unfill-paragraph ()
  "Turns the multiline paragraph to one line."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(global-set-key (kbd "M-Q") 'mz/unfill-paragraph)

;; helper functions to move the current line
(defun mz/move-line-up ()
  "move the current line up 1"
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun mz/move-line-down ()
  "move the current line down 1"
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<down>") 'mz/move-line-down)
(global-set-key (kbd "M-<up>") 'mz/move-line-up)

;;; User Interaction
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

;; hydra provides the ability to create a keybinding menu to reduce redundant keypresses.
;; I also use it for creating restricted, on-demand keymaps.
(use-package hydra
  :bind
  (("C-c f" . hydra-formatting/body)
   ("C-x t" . hydra-tab-bar/body)
   ("C-c e" . hydra-eglot/body)
   ("C-c o" . hydra-org/body)
   ("C-c p" . hydra-projectile/body)))

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
  :bind
  (("M-S-<left>" . windmove-left)
   ("M-S-<right>" . windmove-right)
   ("M-S-<up>" . windmove-up)
   ("M-S-<down>" . windmove-down)

   ("C-M-<left>" . windmove-swap-states-left)
   ("C-M-<right>" . windmove-swap-states-right)
   ("C-M-<up>" . windmove-swap-states-up)
   ("C-M-<down>" . windmove-swap-states-down)))

;;; User Interface
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

;;; Mouse and Keys
(setq-default  scroll-margin 0
               scroll-step 1
               mouse-wheel-progressive-speed nil
               scroll-conservatively 100000
               scroll-preserve-screen-position 1)

;; change font size binding
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; keybinding to reload configuration
(global-set-key (kbd "C-c m") (lambda () (interactive) (load-file "~/.emacs.d/init.el")))

;; keybinding to open configuration file (this file)
(global-set-key (kbd "C-c n") (lambda ()  (interactive) (find-file "~/.emacs.d/init.el")))

;; assume I want to close current buffer with ""C-x k""
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (current-buffer))))

;; shortcut to open eshell in another window. mimics that to open vterm in another window
(global-set-key (kbd "C-M-<return>") 'mz/eshell-other-window)

;;; Languages and LSP Support
;; eglot is a more minimal lsp client
(use-package eglot)

(defhydra hydra-eglot (:color red)
  ("r" eglot-rename "rename")
  ("e" eglot "connect")
  ("d" eglot-find-declaration "declaration")
  ("i" eglot-find-implementation "implementation")
  ("X" eglot-shutdown "shutdown")
  ("R" eglot-reconnect "reconnect")
  ("f" eglot-format "format")
  ("c" eglot-code-actions "code actions")
  ("l" eldoc-doc-buffer "eldoc")

  ("q" nil "exit" :color blue))

;; LSP for C
(add-hook 'c-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; use '//' comments instead of '/* */' comments in C-mode
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; common-lisp environemnt
(use-package slime
  :custom
  (inferior-lisp-program "sbcl"))

;; Help for emacs-lisp functions
(use-package eldoc
  :hook
  ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . eldoc-mode))

;; Elixir major mode hooked up to lsp
(use-package elixir-mode
  :hook (elixir-mode . eglot-ensure))

;; minor mode for mix commands
(use-package mix
  :hook (elixir-mode mix-minor-mode))

(use-package haskell-mode
  :hook (haskell-mode . eglot-ensure))

;; ocaml major mode
(use-package tuareg
  :hook (tuareg-mode . eglot-ensure))

;; dune integration, don't know how to use
(use-package dune)

;; function to build jar from maven project
(defun mz/mvn-jar ()
  "Packages the maven project into a jar."
  (interactive)
  (mvn "package"))

;; function to run the main class defined for the maven project
(defun mz/mvn-run ()
  "Run the maven project using the exec plugin."
  (interactive)
  (mvn "compile exec:java"))

;; function to test all test classes
(defun mz/mvn-test-all ()
  "Run all test classes in the maven project."
  (interactive)
  (mvn "test"))

;; maven minor mode
(use-package mvn
  :bind
  (:map java-mode-map
        (("C-c M" . mvn)
         ("C-c m r" . mz/mvn-run)
         ("C-c m c" . mvn-compile)
         ("C-c m T" . mvn-test) ;; asks for specific test class to run
         ("C-c m t" . mz/mvn-test-all)
         ("C-c m j" . mz/mvn-jar))))

(add-to-list 'eglot-server-programs '(python-mode "pyright-langserver" "--stdio"))

(use-package python
  :hook
  (python-mode . eglot-ensure)
  :custom
  (python-indent-offset 4)
  :config
  (cond
   ;; i use python3
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))))

(use-package racket-mode)

;; tell eglot to use the rust-analyzer binary as the language server
(add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))

;; hook up rust-mode with the language server
(use-package rust-mode
  :custom
  (rust-format-on-save t)
  :hook (rust-mode . eglot-ensure))

;; cargo minor mode for cargo keybindings
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; ruby
(use-package ruby-mode)
(use-package robe
  :hook
  (ruby-mode . robe-mode)
  :config
  (add-to-list 'company-backends 'company-robe))

;; unused web development packages
;; (use-package web-mode)
;; (use-package typescript-mode)
;; (use-package tide)

;; keyboard-driven git interface
(use-package magit
  :bind
  ("C-x g" . magit))

;;; Project Management
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

;; hydra bindings for projectile
(defhydra hydra-projectile (:color red)
  "PROJECTILE: %(projectile-project-root)"

  ("f"  projectile-find-file "file" :column "Find File")
  ("r"   projectile-recentf "recent file")
  ("d"   projectile-find-dir "dir")

  ("b"   projectile-switch-to-buffer "switch to buffer" :column "Buffers")
  ("i"   projectile-ibuffer "ibuffer")
  ("K"   projectile-kill-buffers "kill all buffers")
  ("e"   projectile-run-eshell "eshell" :color blue)

  ("c"   projectile-invalidate-cache "clear cache" :column "Cache (danger)")
  ("x"   projectile-remove-known-project "remove known project")
  ("X"   projectile-cleanup-known-projects "cleanup projects")
  ("z"   projectile-cache-current-file "cache current project")

  ("a"   projectile-ag "ag" :column "Project")
  ("p"   projectile-switch-project "switch project" :column "Project" :color blue)

  ("q"   nil "exit" :color blue))

;;; Text Files
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

(use-package markdown-mode)

;; wraps visual lines
(global-visual-line-mode)

(setq-default
 ;; newline at end of file
 require-final-newline t
 ;; wrap lines at 80 characters
 fill-column 100)

;; delete trailing whitespace when saving.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; function for toggling comments
(defun mz/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

;; binding toggle-comment to "C-."
(global-set-key (kbd "C-.") 'mz/comment-or-uncomment-region-or-line)

;; function to untabify buffer
(defun mz/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;; hydra for formatting files
(defhydra hydra-formatting (:color blue)
  "formatting"
  ("f" format-all-buffer "format-all")
  ("u" mz/untabify-buffer "untabify"))

;;; Org Mode
(setq org-directory "~/org")

;; bullets instead of asterisks
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(setq
 ;; org src blocks act more like the major mode
 org-src-fontify-natively t
 org-src-tab-acts-natively t

 ;; editing source block in same window
 org-src-window-setup 'current-window

 org-support-shift-select t
 org-replace-disputed-keys t)

;; for the "old-school" <s-<tab> to make src blocks
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; change tabs from org-mode
(with-eval-after-load 'org
  (define-key org-mode-map [(control tab)] 'tab-bar-switch-to-next-tab))

;; select the current cell of an org mode table
(defun mz/org-table-select-cell ()
  "select the current table cell"
  ;; do not try to jump to the beginning of field if the point is already there
  (when (not (looking-back "|[[:blank:]]?"))
    (org-table-beginning-of-field 1))
  (set-mark-command nil)
  (org-table-end-of-field 1))

;; copy the current cell of an org mode table
(defun mz/org-table-copy-cell ()
  "Copy the current table field."
  (interactive)
  (mz/org-table-select-cell)
  ;; non-nil third argument copies the current region
  (kill-ring-save 0 0 t)
  (org-table-align))

;; kill the current cell of an org mode table
(defun mz/org-table-kill-cell ()
  "Kill the current table field."
  (interactive)
  (mz/org-table-select-cell)
  ;; non-nil third argument kills the current region
  (kill-region 0 0 t)
  (org-table-align))

(define-key org-mode-map (kbd "S-SPC") 'mz/org-table-copy-cell)
(define-key org-mode-map (kbd "M-S-SPC") 'mz/org-table-kill-cell)

;; general keybindings for org mode
(defhydra hydra-org (:color red)
  "orgmode"
  ("c" org-capture "capture")
  ("a" org-agenda "agenda")
  ("p" org-projectile-project-todo-completing-read "projectile")
  ("q" nil "exit" :color blue))

(setq org-agenda-files (append org-agenda-files '("~/org")))

(global-set-key (kbd "C-c C") 'org-capture)
(setq org-capture-templates '())
;; helper function to add a template to org-capture-templates
(defun mz/add-capture-template (template)
  (let ((key (car template)))
    (setq org-capture-templates
          (cl-remove-if (lambda (x) (equal (car x) key)) org-capture-templates))
    (add-to-list 'org-capture-templates
                 template)))

;; abstracted template for a TODO to take place on some day, like an assignment due date.
(defun mz/todo-on-day-template ()
  "* TODO %? %^t")

;; put a todo file in the directory of each projectile project and link them to org-agenda
(use-package org-projectile
  :custom
  (org-projectile-per-filepath "todo.org")
  :config
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (org-projectile-per-project))

;;; Tramp
(require 'tramp)

(setq tramp-default-method "ssh")

;; helper function to sudo a file
(defun mz/sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

;; attempt to speed things up
(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)

(add-to-list 'tramp-remote-path "~/.local/bin")

;;; VTerm
(setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")

(defun mz/vterm-other-window ()
  "Different vterm sessions for different working directories"
  (interactive)
  (vterm-other-window (concat "vterm: "default-directory)))

;; preferred emacs terminal emulator
(use-package vterm
  :bind
  ("M-RET" . 'mz/vterm-other-window)
  :custom
  (vterm-buffer-name-string "vterm %s")
  :config
  ;; if the fish shell is installed, use that for VTerm's shell
  (when (executable-find "fish")
    (setq vterm-shell (executable-find "fish"))))

;;; Elfeed RSS Reader
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

;;; LaTeX
;; package for editing TeX files
(use-package auctex
  :defer t
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :custom
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-source-correlate-mode t)

  ;; pdf mode
  (TeX-PDF-mode t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (TeX-source-correlate-start-server t)

  (reftex-plug-into-AUCTeX t)
  (TeX-error-overview-open-after-TeX-run t)
  :config
  ;; to have the buffer refresh after compilation. can't be in :hook since it's not a mode hook
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

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

;;; EXWM
;; should exwm be enabled?
(setq exwm-enabled (and (eq window-system 'x)
                        (seq-contains command-line-args "--use-exwm")))

;; package which allows emacs to be a full X11 window manager
(use-package exwm
  :if exwm-enabled
  :init
  ;; package to manage bluetooth from emacs
  (use-package bluetooth)
  ;; enhanced firefox support in exwm
  (use-package exwm-firefox-core
    :if exwm-enabled
    :config
    (require 'exwm-firefox))
  ;; mode to bind media keys
  (use-package desktop-environment
    :custom
    ;; for some reason the default volume commands do not work
    (desktop-environment-volume-toggle-command       "amixer -D pulse set Master toggle")
    (desktop-environment-volume-set-command          "amixer -D pulse set Master %s")
    (desktop-environment-volume-get-command          "amixer -D pulse get Master")
    ;; brightness change amount
    (desktop-environment-brightness-normal-increment "5%+")
    (desktop-environment-brightness-normal-decrement "5%-")
    (desktop-environment-brightness-small-increment  "2%+")
    (desktop-environment-brightness-small-decrement  "2%-"))
  :custom
  (exwm-workspace-number 2)
  (exwm-randr-workspace-monitor-plist
   '(0 "eDP-1" ;; laptop
       1 "DP-3")) ;; external monitor via HDMI which is for some reason named DP-3
  ;; these keys should always pass through to emacs
  (exwm-input-prefix-keys
   '(?\C-x
     ?\C-u
     ?\C-h
     ?\C-g
     ?\M-x
     ?\M-!))
  ;; set up global key bindings.  these always work, no matter the input state!
  ;; keep in mind that changing this list after EXWM initializes has no effect.
  (exwm-input-global-keys
   `(
     ;; reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
     ([?\s-r] . exwm-reset)

     ;; general app launcher
     ;; got rid of counsel
     ;; ([?\s-/] . (lambda ()
     ;; (interactive)
     ;; (counsel-linux-app)))

     ;; shortcut for firefox
     ([?\s-x] . (lambda ()
                  (interactive)
                  (shell-command "firefox")))

     ;; shortcut for terminal emulator
     ([s-return] . (lambda ()
                     (interactive)
                     (vterm-other-window)))))
  :config
  (desktop-environment-mode)
  ;; when window "class" updates, use it to set the buffer name
  (defun mz/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))
  (add-hook 'exwm-update-class-hook #'mz/exwm-update-class)

  ;; enable the next key to be sent directly, for things like copy and paste from x windows
  (define-key exwm-mode-map [?\C-m] 'exwm-input-send-next-key))

;; function to turn on all the exwm stuff
(defun mz/enable-exwm ()
  "Enables the features of EXWM."

  ;; ensure screen updates with xrandr will refresh EXWM frames
  (require 'exwm-randr)
  (exwm-randr-enable)

  ;; use default super+shift keybindings
  (windmove-swap-states-default-keybindings)

  ;; remap capsLock to ctrl
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/xmodmap")

  ;; display time
  (setq display-time-default-load-average nil)
  (display-time-mode t)

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; systray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; enhanced support for firefox
  (exwm-firefox-mode)

  (exwm-enable)
  (exwm-init))

(if exwm-enabled (mz/enable-exwm) ())

;;; _
(provide 'init)
;;; init.el ends here
