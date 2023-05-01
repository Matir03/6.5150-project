(load "./variant.scm")

(define (augment func alist)
  (lambda args
    (let (table (apply func args))
      (for-each
        (lambda (pair)
          (hash-table-set! table (car pair) (cdr pair)))
        alist)
      table)))

(define (on-slice keys func)
  (lambda (state)
    (define values
      (map
        (lambda (key)
          (hash-table-ref table key))
        keys))
    (define new-values
      (apply func values))
    (for-each
      (lambda (key value)
        (hash-table-set! state key value))
      keys new-values)
    state))

(define (players n)
  (lambda (initializer reducer metadata)
    (let ((new-initializer
            (augment initializer '((turn . 0))))
          (new-metadata
            `((end-turn .
                ,(on-slice '(turn)
                  (lambda (turn)
                    (list (mod (+ turn 1) n))))) .
              ((num-players . ,n) . ,metadata))))
      (make-variant
        new-initializer
        reducer
        new-metadata))))
