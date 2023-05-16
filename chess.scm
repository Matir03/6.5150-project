(load "./modifiers.scm")

(define-record-type <movement>
  (make-movement
    is-legal
    moves)
  movement?
  (is-legal legal?)
  (moves get-moves))

(define ((pop op) p1 p2)
  (cons (op (car p1) (car p2))
        (op (cdr p1) (cdr p2))))

(define (short-range diffs)
  (make-movement
    (lambda (s1 s2 color board)
      (define diff ((pop -) s1 s2))
      (and (member diff diffs)
           (not (eq? color (cdr (alist-ref s2 board))))))
    (lambda (s color board)
      (map
        (lambda (diff) ((pop +) s diff))
        diffs))))

(define (long-range diffs)
  (make-movement
    (lambda (s1 s2 color board)
      (define diff ((pop -) s1 s2))
      (define base-diff
        (member diff diffs
          (lambda (d1 d2)
            (eq? (* (car d1) (cdr d2))
                 (* (cdr d1) (car d2))))))
      (define (legal-from s)
        (define next-s ((pop +) s diff))
        (define next-color (cdr (alist-ref next-s board)))
        (and (not (eq? next-color color))
              (or (equal? next-s s2)
                  (and (not next-color)
                      (legal-from next-s)))))
      (legal-from s1))
    (lambda (s color board)
      (append-map
        (lambda (diff)
          (define (locs-from s0)
            (define next-s ((pop +) s0 diff))
            (define next-square (alist-ref next-s board))
            (if next-square
              (let ((next-color (cdr next-square)))
                (if (eq? next-color color)
                    '()
                    (if next-color
                        (list next-s)
                        (cons next-s (locs-from next-s diff)))))
              '()))
          (locs-from s0))
        diffs))))

; (define (union m1 m2)
;   (make-movement
;     (lambda (s1 s2 color board)
;       (or ((legal? m1) s1 s2 color board)
;           ((legal? m2) s1 s2 color board)))
;     (lambda (s color board)
;       (append
;         ((get-moves m1) s color board)
;         ((get-moves m2) s color board)))))

(define (is-legal-pawn s1 s2 color board)
  (define x1 (car s1))
  (define y1 (cdr s1))
  (define x2 (car s2))
  (define y2 (cdr s2))
  (define target-color (cdr (alist-ref s2 board)))
  (define dy (- 1 (* 2 color)))
  (or
    ; capture
    (and (eq? target-color (- 1 color))
         (= (abs (- x1 x2)))
         (= y2 (+ y1 dy)))
    ; 2-push
    (and (= x1 x2)
         (= y2 (+ y1 (* 2 dy)))
         (not (cdr (alist-ref (cons x1 (+ y1 dy)) board)))
         (not target-color))
    ; 1-push
    (and (= x1 x2)
         (= y2 (+ y1 dy))
         (not target-color))))

(define pawn
  (make-movement
    is-legal-pawn
    (lambda (s color board)
      (define x (car s))
      (define y (cdr s))
      (define dy (- 1 (* 2 color)))
      (define lc (cons (- x 1) (+ y dy)))
      (define rc (cons (+ x 1) (+ y dy)))
      (define f1 (cons x (+ y dy)))
      (define f2 (cons x (+ y (* 2 dy))))
      (filter
        (lambda (s2)
          (is-legal-pawn s s2 color board))
        (list lc rc f1 f2)))))

(define (uniques items)
  (filter
    (lambda (item)
      (eq? item (car (member item items))))
    items))

(define (symmetries x y)
  (uniques
    (append-map
      (lambda (signed-x)
        (append-map
          (lambda (signed-y)
            `((,signed-x . ,signed-y) (,signed-y . ,signed-x)))
          (list y (- y))))
      (list x (- x)))))

(define knight
  (short-range
    (symmetries 1 2)))

(define diag (symmetries 1 1))
(define bishop (long-range diag))

(define straight (symmetries 1 0))
(define rook (long-range straight))

(define all-dirs (append straight diag))
(define queen (long-range all-dirs))
(define king (short-range all-dirs))

(define (make-move s1 s2)
  (lambda (board)
    (list
      (alist-set s2
        (alist-ref s1 board)
        (alist-set s1 '(#f . #f) board)))))

(define (board files ranks)
  (lambda (make-initializer make-reducer make-generator make-scorer current-metadata)
    (hash-table-set! current-metadata 'files files)
    (hash-table-set! current-metadata 'ranks ranks)
    (make-variant
      make-initializer
      make-reducer
      make-generator
      make-scorer
      current-metadata)))

(define (piece name movement)
  (lambda (make-initializer make-reducer make-generator make-scorer current-metadata)
    (define (new-make-reducer metadata)
      (define base-reducer (make-reducer metadata))
      (on-action 'move 3
        (lambda (s1 s2)
          (lambda (state)
            (define board (alist-ref 'board state))
            (define turn (alist-ref 'turn state))
            (define occupant (alist-ref s1 board))
            (if (and (equal? occupant (cons name turn))
                     ((legal? movement) s1 s2 turn board))
                ((sequence
                  (on-state '(board) (make-move s1 s2))
                  (gate (meta-ref metadata 'end-turn)))
                state)
                ((base-reducer (list 'move s1 s2)) state))))
        base-reducer))
    (define ((new-make-generator metadata) state)
      (define board (alist-ref 'board state))
      (append ((make-generator metadata) state)
        (append-map
          (lambda (square)
            (define s1 (car square))
            (define piece-name (cadr square))
            (define color (cddr square))
            (if (eq? piece-name name)
                (map
                  (lambda (s2) (list 'move s1 s2))
                  ((get-moves movement) s2 color board))
                '()))
          board)))
    (make-variant
      make-initializer
      new-make-reducer
      new-make-generator
      make-scorer
      current-metadata)))

(define (initialize-chess-game initial-setup)
  (lambda (make-initializer make-reducer make-generator make-scorer current-metadata)
    (define ((new-make-initializer metadata))
      (define board
        (append-map
          (lambda (rank y)
            (map
              (lambda (square x)
                (cons (cons x y) square))
              rank
              (iota (length rank))))
          initial-setup
          (iota (length initial-setup))))
      (cons (cons 'board board) ((make-initializer metadata))))
    (make-variant
      new-make-initializer
      make-reducer
      make-generator
      make-scorer
      current-metadata)))

(define empty-rank
  (map (lambda (index) '(#f . #f)) (iota 8)))

(define chess
  (build-variant
    (players 2)

    ; (board 8 8)

    (piece 'p pawn)
    (piece 'n knight)
    (piece 'b bishop)
    (piece 'r rook)
    (piece 'q queen)
    (piece 'k king)

    (initialize-chess-game
      `(((r . 0) (n . 0) (b . 0) (q . 0) (k . 0) (b . 0) (n . 0) (r . 0))
        ((p . 0) (p . 0) (p . 0) (p . 0) (p . 0) (p . 0) (p . 0) (p . 0))
        ,empty-rank
        ,empty-rank
        ,empty-rank
        ,empty-rank
        ((p . 1) (p . 1) (p . 1) (p . 1) (p . 1) (p . 1) (p . 1) (p . 1))
        ((r . 1) (n . 1) (b . 1) (q . 1) (k . 1) (b . 1) (n . 1) (r . 1))))

    progressive))
