(define pawn)

(define knight
  (short-range
    (symmetries '(2 . 1))))

(define bishop
  (long-range
    (symmetries '(1 . 1))))

(define rook
  (long-range
    (symmetries '(1 . 0))))

(define queen
  (union rook bishop))

(define king
  (short-range
    (append
      (symmetries '(1 . 0)
      (symmetries '(1 . 1))))))

(define empty-rank
  (map (lambda (index) #f) (iota 8)))

(define chess
  (build-variant
    (players 2)

    (piece 'p pawn)
    (piece 'n knight)
    (piece 'b bishop)
    (piece 'r rook)
    (piece 'q queen)
    (piece 'k king)

    (end-condition king-capture)

    (board 8 8)

    (initialize-chess-game
      `(((r . 0) (n . 0) (b . 0) (q . 0) (k . 0) (b . 0) (n . 0) (r . 0))
        ((p . 0) (p . 0) (p . 0) (p . 0) (p . 0) (p . 0) (p . 0) (p . 0))
        ,empty-rank
        ,empty-rank
        ,empty-rank
        ,empty-rank
        ((p . 1) (p . 1) (p . 1) (p . 1) (p . 1) (p . 1) (p . 1) (p . 1))
        ((r . 1) (n . 1) (b . 1) (q . 1) (k . 1) (b . 1) (n . 1) (r . 1))))
    ))