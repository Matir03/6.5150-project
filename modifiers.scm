(load "./variant.scm")

(define ((prepend pairs) alist)
  (append pairs alist))

(define (on-state keys func)
  (lambda (state)
    (define values
      (apply func
        (map cdr
          (filter
            (lambda (entry)
              (memq (car entry) keys))
            state))))
    (if values
      (append
        (map cons keys values)
        (remove
          (lambda (entry)
            (memq (car entry) keys))
          state))
      #f)))

(define (on-action case n func default)
  (lambda (action)
    (if (and (list? action)
             (= (length action) n)
             (eq? (car action) case))
      (apply func (cdr action))
      (default action))))

(define (sequence . funcs)
  (reduce
    (lambda (f g) (lambda args (f (apply g args))))
    #f
    funcs))

(define (meta-ref metadata key)
  ((hash-table-ref metadata key) metadata))

(define (players n)
  (lambda (make-initializer make-reducer make-generator make-scorer current-metadata)
    (define (new-make-initializer metadata)
      (sequence
        (make-initializer metadata)
        (prepend '((turn . 0)))))
    (hash-table-set! current-metadata 'num-players n)
    (hash-table-set! current-metadata 'get-turn
      (ignore1
        (lambda (state)
          (alist-ref 'turn state))))
    (hash-table-set! current-metadata 'set-turn
      (ignore1
        (lambda (turn state)
          (alist-set 'turn turn state))))
    (hash-table-set! current-metadata 'end-turn
      (lambda (metadata)
        (lambda (state)
          (let ((turn ((meta-ref metadata 'get-turn) state))
                (n (hash-table-ref metadata 'num-players))
                (set-turn (meta-ref metadata 'set-turn)))
            (set-turn (modulo (+ turn 1) n) state)))))
    (make-variant
      new-make-initializer
      make-reducer
      make-generator
      make-scorer
      current-metadata)))

(define alist-ref (sequence assq cdr))
(define (alist-set key value alist)
  (cons (cons key value)
        (del-assq key alist)))

(define ((gate f) x)
  (and x (f x)))

(define (nim-stack make-initializer make-reducer make-generator make-scorer current-metadata)
  (define (new-make-reducer metadata)
    (on-action 'take 2
      (lambda (n)
        (sequence
          (on-state '(stack-size)
            (lambda (stack-size)
              (and (integer? n)
                   (> n 0)
                   (<= n stack-size)
                   (list (- stack-size n)))))
          (gate (meta-ref metadata 'end-turn))))
      (make-reducer metadata)))
  (define (new-make-generator metadata)
    (lambda (state)
      (map
        (lambda (n) (list 'take n))
        (iota (alist-ref state 'stack-size) 1))))
  (make-variant
    make-initializer
    new-make-reducer
    new-make-generator
    make-scorer
    current-metadata))

(define (finite-game-sum make-initializer make-reducer make-generator make-scorer current-metadata)
  (define ((new-make-initializer metadata))
    (let ((game ((make-initializer metadata))))
      `((num-games . 1)
        (game-0 . (del-assq game 'turn)))))
  (define base-reducer (make-reducer current-metadata))
  (define (new-make-reducer metadata)
    (on-action 'in 3
      (lambda (index action)
        (define game-name (symbol-append 'game- index))
        (lambda (state)
          (and (assq game-name state)
            (let ((game-state
                    ((base-reducer action)
                     (alist-set 'turn ((meta-ref metadata 'get-turn) state)
                       (alist-ref game-name state)))))

              (and game-state
                (alist-set
                  game-name
                  (del-assq 'turn game-state)
                  (alist-set 'turn (alist-ref 'turn game-state) state)))))))
      no-legal))
  (define base-generator (make-generator current-metadata))
  (define ((new-make-generator metadata) state)
    (apply append
      (map
        (lambda (index)
          (base-generator (alist-ref (symbol-append 'game- index) state)))
        (iota (alist-ref 'num-games table)))))
  (define new-metadata (make-eq-hash-table))
  ((players (hash-table-ref current-metadata 'num-players))
      new-make-initializer
      new-make-reducer
      new-make-generator
      make-scorer
      new-metadata))

(define (initialize-to state)
  (lambda (make-initializer make-reducer make-generator make-scorer current-metadata)
    (make-variant
      (ignore2 state)
      make-reducer
      make-generator
      make-scorer
      current-metadata)))

(define (initialize-nim-game initial-stacks)
  (initialize-to
    (append
      '((num-games . 4)
        (turn . 0))
      (map
        (lambda (stack-size index)
          (cons (symbol-append 'game- index)
                `((stack-size . ,stack-size))))
        initial-stacks
        (iota (length initial-stacks))))))