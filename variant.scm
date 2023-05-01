(define-record-type <variant>
  (make-variant
    initializer ; () => State
    reducer     ; (State Action) => State | #f ; (if the action fails)
    ; renderer    ; (State) => renders state
    metadata)   ; arbitrary association list
  variant?
  (initializer get-initializer)
  (reducer get-reducer)
  ; (renderer get-renderer)
  (metadata get-metadata))

(define the-null-variant
  (make-variant
    (lambda () (make-eq-hash-table))
    (lambda (state action) #f)
    ; (lambda (state)
    ;   (begin
    ;     (display state)
    ;     (newline)))
    '()))

(define (initial-state variant)
  ((get-initializer variant)))

(define (pretty-state state)
  (hash-table->alist state))

; possibly make this variant dependent as well
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
  (let* ((reducer (get-reducer variant))
         (new-state (reducer state action)))
    (if new-state
        (make-outcome "Successful action!" new-state)
        (make-outcome "Illegal action!" state))))

; build variants
(define (apply-modifier modifier variant)
  (let ((initializer (get-initializer variant))
        (reducer (get-reducer variant))
        (metadata (get-metadata variant)))
    (modifier initializer reducer metadata)))

(define (build-variant . modifiers)
  (build-variant* modifiers))

(define (build-variant* modifiers)
  (fold apply-modifier the-null-variant modifiers))