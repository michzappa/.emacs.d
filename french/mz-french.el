;; Utilities to read a FR-EN dictionary file. Heavily inspired by Josh Moller-Mara's Chinese functions.
;; - Michael Zappa

(defvar mz/french-dictionary-path
  ;; (concat (file-name-directory load-file-name) "fr-en.txt")
  "/home/michael/.emacs.d/french/fr-en.txt"
  "Where the French-English dictionary is stored.")

(defun mz/french-prompt ()
  "Prompt for a french word/phrase, then return it"
  (setq mz/french-word (read-from-minibuffer "Word/Phrase: ")))

(defun mz/french-dict-find (phrase)
  "Find a french word or phrase in the dictionary"
  (with-temp-buffer
    (insert-file-contents mz/french-dictionary-path)
    (let (definitions)
      (while (re-search-forward
              (concat
               "^"
               phrase
               ".*\t.*$")
               nil t)
        (push (buffer-substring (match-beginning 0)
                                (match-end 0))
              definitions))
      (setq mz/french-word-dict
            (if (equal (length definitions) 1)
                (car definitions)
              (ivy-read "Pick a definition: "
                        definitions))))))

(defun mz/french-definition (dictentry)
  "Get English definition from a dictionary entry"
  (car (cdr (split-string dictentry "\t" t))))

(defun mz/french-part-of-speech (dictentry)
  "Ge the part of speech for the given dictionary entry"
  (car (cdr (cdr (split-string dictentry "\t" t)))))

(provide 'mz-french)
