(load "./variant.scm")

(define (copy-hash-table hash-table)
  ;;hash-table)
  (alist->hash-table (hash-table->alist hash-table)))

(define (unsafe-perform-action variant state action)
  (let* ((reducer (get-reducer variant))
         (new-state ((reducer action) state)))
    new-state))

; Returns a list of (state . move)
(define (generate-states variant state)
  (let* ((generator (get-generator variant))
	 (possible-actions (generator state))
	 (possible-states (map (lambda (action) (unsafe-perform-action variant (alist-copy state) action))
			       possible-actions)))
    (filter values possible-states))) ; Values is an identity funtion to remove states which are #f

(define (generate-moves-and-states variant state)
  (let* ((action-generator (get-generator variant))
	 (possible-actions (action-generator state))
	 (possible-states (map (lambda (action) (unsafe-perform-action variant (alist-copy state) action))
			       possible-actions))
	 (possible-actions-states (map list possible-actions possible-states)))
    (filter cadr possible-actions-states))) ; Filter out states which are #f

(define (get-player-count variant)
  (let ((metadata (get-metadata variant)))
    (hash-table-ref metadata 'num-players)))

(define (get-player metadata)
  ((hash-table-ref metadata 'get-turn)))

(define (is-maximizing-player? player state variant)
  (let ((metadata (get-metadata variant)))
    (eq? ((get-player metadata) state) player)))

(define (get-is-promising variant)
  (let ((metadata (get-metadata variant)))
    (hash-table-ref metadata 'is-promising)))

(define (get-update-search-data variant)
  (let ((metadata (get-metadata variant)))
    (hash-table-ref metadata 'update-search-data)))

(define (get-move-decider variant)
  (let ((metadata (get-metadata variant)))
    (hash-table-ref metadata 'move-decider)))

(define (convert-to-move-picker move-decider)
  (lambda (move1 move2) (if (move-decider move1 move2) move1 move2)))

;;  (get-best-move-min-max variant state 3))
(define (get-score-multiplayer-internal variant state action depth maximizing-player search-data)
  (define is-promising (get-is-promising variant))
  (define move-decider (get-move-decider variant))
  (define update-search-data (get-update-search-data variant))
  (let ((is-maximizing-player (is-maximizing-player? maximizing-player state variant)))
    (if (= depth 0)
	(get-score variant state maximizing-player)
	(let ((possible-action-states (generate-moves-and-states variant state)))
	  (reduce (convert-to-move-picker (move-decider variant state action maximizing-player))
	          (get-score variant state maximizing-player)
		  (map (lambda (possible-action-state)
			 (if (is-promising search-data (cadr possible-action-state) (car possible-action-state))
			    (update-search-data (get-score-multiplayer-internal
				variant
				(cadr possible-action-state)
				(car possible-action-state)
				(- depth 1)
				maximizing-player
				search-data) search-data)))
		    possible-action-states))))))

(define (generic-bot-builder is-promising update-search-data move-decider metadata)
    (hash-table-set! metadata 'simulation #t)
    (hash-table-set! metadata 'is-promising is-promising)
    (hash-table-set! metadata 'update-search-data update-search-data)
    (hash-table-set! metadata 'move-decider move-decider))

(define (get-move-pairs variant state depth)
  (define search-data (make-eq-hash-table))
  (if (< depth 1)
      (error "cannot get a best move with depth < 1")
      (let* ((maximizing-player ((get-player (get-metadata variant)) state))
	     (possible-actions-states (generate-moves-and-states variant state))
	     (scores (map (lambda (action-state-pair)
			    (get-score-multiplayer-internal variant
					       (cadr action-state-pair)
					       (car action-state-pair)
					       (- depth 1)
					       maximizing-player
					       search-data))
			  possible-actions-states))
	     (actions-values (map (lambda (action-state score)
				    (list (car action-state) score))
				  possible-actions-states scores)))
	actions-values)))

(define ((sort-action-values move-decider) action-values)
  (sort action-values move-decider cadr))

(define (get-which-move variant state action depth)
  (define maximizing-player ((get-player (get-metadata variant)) state))
  (define move-decider (get-move-decider variant))
  (define move-sorter (move-decider variant state '() maximizing-player))
  (let* ((actions-values ((sort-action-values move-sorter)
			  (get-move-pairs variant state (* depth (get-player-count variant)))))
	 (actions (map car actions-values)))
    (- (length actions) (length (member action actions)))))

(define (get-nth-best-move-multiplayer variant state depth n)
  (define maximizing-player ((get-player (get-metadata variant)) state))
  (define move-decider (get-move-decider variant))
  (define move-sorter (move-decider variant state '() maximizing-player))
  (let* ((actions-values ((sort-action-values move-sorter) (get-move-pairs variant state (get-player-count variant))))
	 (actions (map car actions-values)))
    (if (<= (length actions) n)
	(list-ref actions 0)
	(list-ref actions n))))
		
(define (get-nth-best-move variant state n depth)
  (get-nth-best-move-multiplayer variant state (* depth (get-player-count variant)) n))

(define (get-score variant state maximizing-player)
  (let* ((score-getter (get-scorer variant)))
    (score-getter  state maximizing-player)))

(define (max-with-inf a b)
  (cond ((eq? a '-inf) #f)
	((eq? a 'inf) #t)
	((eq? b '-inf) #t)
	((eq? b 'inf) #f)
	(else (> a b))))

(define (min-with-inf a b)
  (not (max-with-inf a b)))
