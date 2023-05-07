(load "./variant.scm")

(define (copy-hash-table hash-table)
  (alist->hash-table (hash-table->alist hash-table)))

; Returns a list of (state . move)
(define (generate-states variant state)
  (let* ((generator (get-generator variant))
	 (reducer (get-reducer variant))
	 (possible-actions (generator state))
	 (possible-states (map (lambda (action) (reducer (copy-hash-table state) action))
			       possible-actions)))
    (filter values possible-states))) ; Values is an identity funtion to remove states which are #f

(define (generate-moves-and-states variant state)
  (let* ((action-generator (get-generator variant))
	 (reducer (get-reducer variant))
	 (possible-actions (action-generator state))
	 (possible-states (map (lambda (action) (reducer (copy-hash-table state) action))
			       possible-actions))
	 (possible-actions-states (map cons possible-actions possible-states)))
    (filter cadr possible-actions-states))) ; Filter out states which are #f

(define (max-with-inf-and-values a b)
  (if (eq? (cadr a) '-inf)
      b
      (if (> (cadr a) (cadr b))
	  a
	  b)))
(define (get-player-count x) 2)

;;  (get-best-move-min-max variant state 3))
(define (get-score-multiplayer-internal variant state action depth maximizing-player)
  (let ((is-maximizing-player (is-maximizing-player? maximizing-player state)))
    (if (= depth 0)
	(get-score variant state action is-maximizing-player)
	(let ((possible-action-states (generate-moves-and-states variant state)))
	  (reduce (if is-maximizing-player max-with-inf min-with-inf)
		  (get-score variant '() action is-maximizing-player)
		  (map (lambda (possible-action-state)
			(get-score-multiplayer-internal variant (cadr possible-state) (car possible-state) (- depth 1)))
		    possible-states))))))

(define (get-best-move-multiplayer variant state depth)
  (if (< depth 1)
      (error "cannot get a best move with depth < 1")
      (let* ((possible-actions-states (generate-moves-and-states variant state))
	     (scores (map (lambda (action-state-pair)
			    (get-score-multiplayer-internal variant
					       (cadr action-state-pair)
					       (car action-state-pair)
					       (- depth 1)
					       (get-player (cadr action-state-pair))))
			  possible-actions-states))
	     (actions-values (map (lambda (action-state score)
				    (cons (car action-state) score))
				  possible-actions-states scores)))
	(if (pair? possible-actions-states)
	    (car (reduce max-with-inf-and-values '(test -inf) actions-values))
	    (error "No moves can be made")))))

(define (is-maximizing-player? player state)
  (eq? (get-player state) player))

		
(define (get-best-move variant state)
  (get-best-move-multiplayer variant state (get-player-count variant)))


(define (get-score variant state)
  (let* ((score-getter (state-score-calculator variant)))
    (score-getter state)))

(define (max-with-inf a b)
  (cond ((eq? a '-inf) b)
	((eq? a 'inf) a)
	((eq? b '-inf) a)
	((eq? b 'inf) b)
	(else (max a b))))

(define (min-with-inf a b)
  (cond ((eq? a '-inf) a)
	((eq? a 'inf) b)
	((eq? b '-inf) b)
	((eq? b 'inf) a)
	(else (min a b))))

#|(define (get-best-move-min-max variant state depth)
  (if (< depth 1)
      (error "cannot get a best move with depth < 1")
      (let* ((possible-actions-states (generate-moves-and-states variant state))
	     (scores (map (lambda (action-state-pair)
			    (get-score-min-max-internal variant
					       (cadr action-state-pair)
					       (- depth 1) #f))
			  possible-actions-states))
	     (actions-values (map (lambda (action-state score)
				    (cons (car action-state) score))
				  possible-actions-states scores)))
	(if (pair? possible-actions-states)
	    (car (fold-right max-with-inf-and-values '('test '-inf) actions-values))
	    (error "No moves can be made")))))
	    |#

#|
;; Return just the score of the move given state
(define (get-score-min-max-internal variant state depth maximizing-player)
  ;; First step is to generate all the legal moves
  (if (= depth 0)
      (get-score variant state)
      (let ((possible-states (generate-states variant state)))
	(if (pair? possible-states)
	    (case maximizing-player
	      (#t (fold-right max-with-inf '-inf
			      (map (lambda (new-state) (get-score-min-max-internal
							variant new-state (- depth 1) #f))
				   possible-states)))
	      (#f (fold-right min-with-inf 'inf
			      (map (lambda (new-state) (get-score-min-max-internal
							variant new-state (- depth 1) #t))
				   possible-states))))
	    (get-score variant state)))))

|#
