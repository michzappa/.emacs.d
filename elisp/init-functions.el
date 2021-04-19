;; init-functions

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


;; function to untabify buffer
(defun mz/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(provide 'init-functions)
;; init-functions ends here
