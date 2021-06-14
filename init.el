(org-babel-load-file (expand-file-name "README.org" "~/.emacs.d"))
(when (file-exists-p custom-file)
  (load-file custom-file))
