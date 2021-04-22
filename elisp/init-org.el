;; init-org

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

;; minor mode for working with org mode tables, to separate the keybindings
;; because they are often annoying when just using org mode for word processing
(define-minor-mode mz/org-table
  "Toggle helpful keybindings for working with org mode tables"
  :init-value
  nil
  :lighter
  "mz/org-table"
  :keymap
  '(([S-SPC] . mz/org-table-copy-cell)
    ([M-S-SPC] . mz/org-table-kill-cell)))

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

(provide 'init-org)
;; init-org.el ends here
