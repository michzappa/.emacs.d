;; init-tramp

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

(provide 'init-tramp)
;; init-tramp.el ends here
