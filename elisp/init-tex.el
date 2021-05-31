;; init-tex

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

(provide 'init-tex)
;; init-tex.el ends here
