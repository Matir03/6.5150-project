(load "./variant.scm")

(define (add-to-all func alist)
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
    (if new-values
      (begin
        (for-each
          (lambda (key value)
            (hash-table-set! state key value))
          keys new-values)
        state)
      #f)))

(define (partial case func default)
  (lambda (action)
    (if (eq? (car action) case)
      (apply func (cdr action))
      (default action))))

(define (sequence . funcs)
  (reduce
    (lambda (f g) (lambda args (f (g args))))
    (lambda args args)
    funcs))

(define (players n)
  (lambda (initializer reducer generator scorer metadata)
    (let ((new-initializer
            (add-to-all initializer '((turn . 0))))
          (new-metadata
            `((end-turn .
                ,(on-slice '(turn)
                  (lambda (turn)
                    (list (mod (+ turn 1) n)))))
              (num-players . ,n)
              . ,metadata)))
      (make-variant
        new-initializer
        reducer
        generator
        scorer
        new-metadata))))

(define (get-player state)
  (error "Not yet implemented"))

(define (bot . which-players)
  (lambda (initializer reducer generator scorer metadata)
    (let ((new-reducer
	   (lambda (state action)
	     ;; If the next player is a bot, then only accept the move bot-move and actually perform a bot move.
	     ;; Can only be applied with a players n variant
	     (cond ((memq (get-player state) which-players)
		    (if (eq? action 'bot-move)
			(reducer state (get-best-move (make-variant initializer reducer generator scorer metadata) state))
			(reducer state action)))
		   (else (reducer state action)))))
          (new-metadata
            `((bot . ,which-players)
              . ,metadata)))
      (make-variant
        initializer
        new-reducer
	generator ;; We do not edit the generator since we don't want bot-moves to show up
        scorer
        new-metadata))))

(define (nim-stack initializer reducer generator scorer metadata)
  (let ((new-reducer
          (sequence
            (partial 'take
              (lambda (n)
                (on-slice '(stack-size)
                  (lambda (stack-size)
                    (and (integer? n)
                        (<= n stack-size)
                        (list (- stack-size n))))))
              reducer)
            (cdr (assq 'end-turn metadata))))
        (new-generator
          (lambda (state)
            (map
              (lambda (n) (list 'take n))
              (iota (+ (hash-table-ref state 'stack-size) 1)))))
    (make-variant
      initializer
      new-reducer
      new-generator
      scorer
      metadata))))

(define (finite-game-sum initializer reducer generator scorer metadata)
  (define (new-initializer)
    (define table (new-eq-hash-table))
    (hash-table-set! table 'num-games 1)
    (hash-table-set! table (+ 'game 0) (initializer)))
  (define new-reducer
    (partial 'in
      (lambda (index action)
        (lambda (state)
          ((reducer action)
            (hash-table-ref state (+ 'game index))))))
    no-legal)
  (define (new-generator state)
    (define num-games (hash-table-ref table 'num-games))
    (apply append
      (map
        (lambda (index)
          (generator (hash-table-ref table (+ 'game index))))
        (iota num-games)))))
