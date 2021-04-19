;; init-vterm

;; download libvterm when building this package as opposed to installing it on the system
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

(provide 'init-vterm)
;; init-vterm.el ends here
