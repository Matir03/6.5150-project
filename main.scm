(load "./modifiers.scm")

(define (get-action)
  (string->symbol (read-line)))

(define (ended? state)
  #f)

(define (play variant)
  (define (action-loop state)
    (render-state variant state)
    (if (not (ended? state))
      (let* ((action (get-action))
            (outcome (perform-action variant state action))
            (message (get-message outcome))
            (new-state (get-state outcome)))
        (display message)
        (newline)
        (action-loop new-state))))
  (action-loop (initial-state variant)))

(define nim
  (build-variant
    (players 2)
    nim-stack
    finite-game-sum
    (initialize-nim-game
      '(3 4 5 6))))

(play variant)
(define (simple-stack initializer reducer generator scorer metadata)
  (let ((new-initializer (lambda () (let ((state (initializer))) (hash-table-set! state 'stack-size 10) state)))
	(new-reducer (lambda (state action) (hash-table-set! state 'stack-size (cadr action)) state))
        (new-generator (lambda (state) (map (lambda (n) (list 'turn n)) (iota (hash-table-ref state 'stack-size)))))
	(new-scorer (lambda (state action is-maximizing-player) ;; Since the goal for a single stack is to take the last object i.e. shrink the stack size, then I suppose the score should be - of the stack length
		      (if (eq? state '())
			  (if is-maximizing-player 'inf '-inf)
			  (- (hash-table-ref state 'stack-size))))))
		      
    (make-variant
      new-initializer
      new-reducer
      new-generator
      new-scorer
      metadata)))

(define simple-variant
  (build-variant
   (players 2)

   simple-stack
   (bot 0)))
   
