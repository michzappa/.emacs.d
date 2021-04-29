;; init-lang-lsp

;; eglot is a more minimal lsp client
(use-package eglot)
;; (defun project-root (project)
  ;; (car (project-roots project)))
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

;; i use pyright as python lsp
(add-to-list 'eglot-server-programs '(python-mode "pyright-langserver" "--stdio"))

;; options for python-mode
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

;; environment for racket, with REPL
(use-package racket-mode)

;; tell eglot to use the rust-analyzer binary as the language server
(add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))

;; hook up rust-mode with the language server
(use-package rust-mode
  :hook (rust-mode . eglot-ensure))

;; cargo minor mode for cargo keybindings
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(provide 'init-lang-lsp)
;; init-lang-lsp.el ends here
