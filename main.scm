(load "./modifiers.scm")

(define (get-action)
  (string->symbol (read-line)))

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