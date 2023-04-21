(define (initial-state variant)
  '())

(define (render-state variant state)
  (newline)
  (display "Current state: ")
  (display state)
  (newline)
  (display "Enter action: "))

(define make-outcome cons)
(define get-message car)
(define get-state cdr)

(define (perform-action variant state action)
  (make-outcome "Illegal action!" state))
