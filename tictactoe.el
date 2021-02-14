;; tictactoe.el -- play tic tac toe in Emacs

(defun tictactoe ()
  "Start playing tic tac toe"
  (interactive)
  (switch-to-buffer "tictactoe")
  (tictactoe-mode)
  (tictactoe-init))

(define-derived-mode tictactoe-mode special-mode "Tic-Tac-Toe"
  (define-key tictactoe-mode-map (kbd "SPC") 'tictactoe-mark))

(defun tictactoe-init ()
  "Start a new game of tic tac toe."
  (setq tictactoe-board
        (make-vector
         (* tictactoe-size tictactoe-size)
         ?\.))
  (tictactoe-print-board)
  (setq tictactoe-current-player ?\X))

(defconst tictactoe-size 3
  "the size of the board, both height and width")

(defvar tictactoe-board nil
  "The board itself")

(defun tictactoe-print-board ()
  "Prints the contents of tictactoe-board in a 2D format."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (text-scale-set 14)
    (dotimes (row tictactoe-size)
      (dotimes (col tictactoe-size)
        (insert (tictactoe-get-square row col)))
      (insert "\n"))))

(defun tictactoe-get-square (row col)
  "Get the value in the given square of the board."
  (elt tictactoe-board
       (+ col
          (* row tictactoe-size))))

(defun tictactoe-set-square (row col value)
  "Set the given square to be the given value."
  (aset tictactoe-board
        (+ col
           (* row tictactoe-size))
        value))

(defun tictactoe-mark ()
  "Mark the current square."
  (interactive)
  (let ((row (1- (line-number-at-pos)))
        (col (current-column)))
    (message "%d, %d" row col)

    ;; check if square is in bounds
    (if (and
         (and (<= row 2)
              (>= row 0))
         (and (<= col 2)
              (>= col 0)))
        ;; check if square is already marked
        (if (or (char-equal (tictactoe-get-square
                             (1- (line-number-at-pos))
                             (current-column))
                            ?\X)
                (char-equal  (tictactoe-get-square
                              (1- (line-number-at-pos))
                              (current-column))
                             ?\O))
            (message "Cannot mark already marked square.")
          ;; if not already marked, do the things
          (progn
            (tictactoe-set-square row col tictactoe-current-player)
            ;; reprint the board
            (tictactoe-print-board)
            ;; win condition message
            (when (tictactoe-game-won)
              (message "Congrats! Player %c has won!" tictactoe-current-player))
            ;; swap players
            (tictactoe-change-player)))
      (message "Cannot mark out-of-bounds square."))))

(defvar tictactoe-current-player nil
  "The character representing the current player")

(defun tictactoe-change-player ()
  "Make it the other player's turn"
  (setq tictactoe-current-player
        (if (char-equal tictactoe-current-player ?\X)
            ?\O
          ?\X)))

(defun tictactoe-game-won ()
  "Returns t if the game has been won, nil otherwise."
  (or (tictactoe-diag-win)
      (tictactoe-row-win)
      (tictactoe-col-win)))

(defun tictactoe-diag-win ()
  "Returns t if there is a win along either diagonal, nil otherwise."
  (or (tictactoe-all-same-player
       (tictactoe-get-square 0 0)
       (tictactoe-get-square 1 1)
       (tictactoe-get-square 2 2))
      (tictactoe-all-same-player
       (tictactoe-get-square 0 2)
       (tictactoe-get-square 1 1)
       (tictactoe-get-square 2 0))))

(defun tictactoe-row-win ()
  "Returns t if there is a win along any row, nil otherwise."
  (let ((has-won nil))
    (dotimes (row tictactoe-size)
      (when (tictactoe-all-same-player
             (tictactoe-get-square row 0)
             (tictactoe-get-square row 1)
             (tictactoe-get-square row 2))
        (setq has-won t)))
    has-won))

(defun tictactoe-col-win ()
  "Returns t if there is a win along any column, nil otherwise."
  (let ((has-won nil))
    (dotimes (col tictactoe-size)
      (when (tictactoe-all-same-player
             (tictactoe-get-square 0 col)
             (tictactoe-get-square 1 col)
             (tictactoe-get-square 2 col))
        (setq has-won t)))
    has-won))

(defun tictactoe-all-same-player (sq1 sq2 sq3)
  "Returns t if all given squares contain the same player's mark."
  (and (char-equal sq1 sq2)
       (char-equal sq2 sq3)
       (or
        (char-equal sq1 ?\X)
        (char-equal sq1 ?\O))))
