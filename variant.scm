(define-record-type <variant>
  (make-variant
    make-initializer ; (metadata) => () => state
    make-reducer     ; (metadata) => (action) => (state) => state | #f ; (if the action fails)
    make-generator   ; (metadata) => (state) => list action
    make-scorer      ; (metadata) => (state) => score
    metadata)   ; arbitrary hash table
  ; state is an association list
  ; it is assumed to never be mutated
  variant?
  (make-initializer initializer-maker)
  (make-reducer reducer-maker)
  (make-generator generator-maker)
  (make-scorer scorer-maker)
  (metadata get-metadata))

(define ((make-getter maker) variant)
  ((maker variant) (get-metadata variant)))

(define get-initializer (make-getter initializer-maker))
(define get-reducer (make-getter reducer-maker))
(define get-generator (make-getter generator-maker))
(define get-scorer (make-getter scorer-maker))

(define ((ignore1 x) . args) x)
(define (ignore2 x) (ignore1 (ignore1 x)))

(define no-legal (ignore2 #f))

(define the-null-variant
  (make-variant
    (ignore2 '())           ; initializer
    (ignore1 no-legal)      ; reducer
    (ignore2 '())           ; generator
    (ignore2 0)             ; scorer
    (make-eq-hash-table)))  ; metadata

(define (initial-state variant)
  ((get-initializer variant)))

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
         (new-state ((reducer action) state)))
    (if new-state
        (make-outcome "Successful action!" new-state)
        (make-outcome "Illegal action!" state))))

; build variants
(define (apply-modifier modifier variant)
  (let ((make-initializer (initializer-maker variant))
        (make-reducer (reducer-maker variant))
        (make-generator (generator-maker variant))
        (make-scorer (scorer-maker variant))
        (metadata (get-metadata variant)))
    (modifier
      make-initializer
      make-reducer
      make-generator
      make-scorer
      metadata)))

(define (build-variant . modifiers)
  (build-variant* modifiers))

(define (build-variant* modifiers)
  (fold apply-modifier the-null-variant modifiers))
