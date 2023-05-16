(load "./modifiers.scm")

(define (play variant)
  (define (action-loop state)
    (render-state variant state)
    (let* ((action (read))
          (outcome (perform-action variant state action))
          (message (get-message outcome))
          (new-state (get-state outcome)))
      (display message)
      (newline)
      (action-loop new-state)))
  (action-loop (initial-state variant)))

(define nim
  (build-variant
    (players 2)
    nim-stack
    finite-game-sum
    (initialize-nim-game '(3 4 5 6))
    (adaptive-paranoid-bot 1 0 1)))

(play nim)

bot-move
(in 0 (take 2))
bot-move
(in 1 (take 3))
bot-move
(in 0 (take 1))