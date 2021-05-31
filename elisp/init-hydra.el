;; init-hydra

;; hydra provides the ability to create a keybinding menu to reduce redundant keypresses.
;; I also use it for creating restricted, on-demand keymaps.
(use-package hydra
  :bind
  (("C-c f" . hydra-formatting/body)
   ("C-c k" . hydra-ime/body)
   ("C-x t" . hydra-tab-bar/body)
   ("C-c e" . hydra-eglot/body)
   ("C-c o" . hydra-org/body)
   ("C-c p" . hydra-projectile/body)))

;; hydra bindings for projectile
(defhydra hydra-projectile (:color blue)
  "PROJECTILE: %(projectile-project-root)"

  ("f"  projectile-find-file "file" :column "Find File")
  ("r"   projectile-recentf "recent file")
  ("d"   projectile-find-dir "dir")

  ("b"   projectile-switch-to-buffer "switch to buffer" :column "Buffers")
  ("i"   projectile-ibuffer "ibuffer")
  ("K"   projectile-kill-buffers "kill all buffers")
  ("e"   projectile-run-eshell "eshell")

  ("c"   projectile-invalidate-cache "clear cache" :column "Cache (danger)")
  ("x"   projectile-remove-known-project "remove known project")
  ("X"   projectile-cleanup-known-projects "cleanup projects")
  ("z"   projectile-cache-current-file "cache current project")

  ("p"   projectile-switch-project "switch project" :column "Project")

  ("q"   nil "exit"))

;; hydra for formatting files
(defhydra hydra-formatting (:color blue)
  "formatting"
  ("f" format-all-buffer "format-all")
  ("u" mz/untabify-buffer "untabify"))

;; common functions for interacting with the lsp client
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

;; general keybindings for org mode
(defhydra hydra-org (:color red)
  "orgmode"
  ("c" org-capture "capture")
  ("a" org-agenda "agenda")
  ("p" org-projectile-project-todo-completing-read "projectile")
  ("q" nil "exit" :color blue))

;; hydra to change input mode
(defhydra hydra-ime (:color blue)
  "input methods"
  ("i" (lambda () (interactive) (set-input-method "ipa-praat")) "ipa" :column "misc")
  ("a" (lambda () (interactive) (set-input-method "Agda")) "agda")
  ("jj" (lambda () (interactive) (set-input-method "japanese")) "ipa" :column "japanese")
  ("jk" (lambda () (interactive) (set-input-method "japanese-katakana")) "katakana")
  ("c"  (lambda () (interactive) (set-input-method "chinese-py")) "pinyin" :column "chinese"))

(provide 'init-hydra)
;; init-hydra.el ends here
