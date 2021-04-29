;; init-keys

;; reverse the result of fill-paragraph
(global-set-key (kbd "M-Q") 'mz/unfill-paragraph)

;; move current line
(global-set-key (kbd "M-<down>") 'mz/move-line-down)
(global-set-key (kbd "M-<up>") 'mz/move-line-up)

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

;; toggle comment of current line
(global-set-key (kbd "C-.") 'mz/comment-or-uncomment-region-or-line)

(global-set-key (kbd "M-F") 'find-file-at-point)

(provide 'init-keys)
;; init-keys.el ends here
