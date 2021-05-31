(package-initialize)
(org-babel-load-file (expand-file-name "README.org"))
(when (file-exists-p custom-file)
  (load-file custom-file))
