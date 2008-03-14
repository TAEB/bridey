(define (initial-state char)
  (set-state
   (create-level '())
   'nutrition 900
   'encumbrance 0
   'last-command '(#f #f)
   'do-inventory? #t
   'fountains '()
   'thrones '()
   'interesting-squares '()
   'question-handler handle-question
   'pickup-pred (lambda (x) #f)))

(define (create-level state)
  (set-state
   state
   'searched (make-byte-vector (* 80 24) 0)
   'stuck-boulders '()
   'rooms '()
   'searched-walls '()))

(define (process-turn state)
  ; need a macro for this :)
  (let* ((state (set-state state
			   'messages '()
			   'interesting-squares '()))
	 (state (read-topl state))
	 (state (set-state state 'coord (get-coord)))
	 (state (fold process-message state (get-state state 'messages)))
	 (state (if (and (get-state state 'do-look?)
			 (not (get-state state 'blind?)))
		    (do-look (set-state state 'do-look? #f))
		    state))
	 (state (if (get-state state 'do-inventory?)
		    (set-state state
			       'inventory (get-inventory)
			       'do-inventory? #f)
		    state))
	 (state (begin (botl-update) (turns-passed state)))
	 (state (update-map state))
	 (get (specialize get-state state))
	 (set (specialize set-state state))
	 (coord (get-coord)))
    (mark-visited coord)
    (mark-seen coord)
    (if (not (get 'blind?))
	(for-each mark-seen (neighbor-squares coord)))
    (if (and (weird-position? state)
	     (eq? (car (get 'last-command)) 'move))
	(let ((dest (get 'expected-coord)))
	  (cond ((and (char=? (square-char dest) #\^)
		      (eq? (square-color dest) 'magenta)
		      (not (trap? dest)))
		 ; not levelport trap because those always give a message
		 (mark-trap dest 'teleportation))
		((not (square-clear? state dest))
		 (mark-trap dest 'maybe)))))
    (let ((squares-with-mons
	   (filter (lambda (c)
		     (let ((desc (far-look c)))
		       (not (or (string-prefix? "tame " desc)
				(string-prefix? "peaceful " desc)))))
		   (filter (specialize monster? state)
			   (neighbor-squares))))
	  (search (get 'searching-for)))
      (cond ((not (null? squares-with-mons)) (hit-something state))
	    (else (maybe-eat state))))))

(define (turns-passed state)
  (define get (specialize get-state state))
  (define set (specialize set-state state))
  (define (hunger turn)
    (count
     identity
     (list (not (or (get 'slow-digestion?)
		    (and (get 'polymorphed)
			 (monster-breathless? (get 'polymorphed)))))
	   (and (odd? turn) (eq? (get 'regeneration?) 'ring))
	   (and (odd? turn) (> (get 'encumbrance) 1))
	   (and (even? turn) (get 'rapid-hunger?))
	   (and (even? turn) (eq? (get 'conflict?) 'ring))
	      (and (= (modulo turn 20) 4) (get 'left-ring))
	      (and (= (modulo turn 20) 8) (get 'amulet))
	      (and (= (modulo turn 20) 12) (get 'right-ring))
	      (and (= (modulo turn 20) 16) (get 'have-aoy?)))))
  (let ((last (or (get 'last-turn) 1)))
    (set 'nutrition (- (get 'nutrition)
		       (* (if (eq? (car (get 'last-command)) 'fight) 2 1)
			  (apply + (map hunger (range last (- (turns) 1))))))
	 'last-turn (turns))))

(define (hit-something state)
  (let ((squares-with-mons
	 (filter (lambda (c)
		   (let ((desc (far-look c)))
		     (not (or (string-prefix? "tame " desc)
			      (string-prefix? "peaceful " desc)))))
		 (filter (specialize monster? state)
			 (neighbor-squares)))))
    (fight state (map - (car squares-with-mons) (get-coord)))))

;(define (want-item? item)
;  (string=? (item-name item) "food ration"))


(define (draw-seen state)
  (do ((y 1 (+ y 1)))
      ((= y 24))
    (do ((x 1 (+ x 1)))
	((= x 80))
      (display (if (seen? (list x y)) "#" ".")))
    (newline)))

(define (update-map state)
  (iterate-screen
   (lambda (state coord char color)
     (case char
       ((#\space)
	(if (square-covered-by-item? coord)
	    (unmark-square-covered-by-item coord))
	state)
       ((#\I) state)
       (else
	(if (not (seen? coord))
	    (mark-seen coord))
	(cond
	 ((and (eq? color 'blue)
	       (char=? char #\{))
	  (add-fountain state coord))
	 ((and (door? coord)
	       (square-clear? state coord))
	  (unmark-door coord)
	  state)
	 ((item? state coord)
	  (set-square-covered-by coord)
	  (if (visited? coord)
	      state
	      (cons-state state 'interesting-squares coord)))
	 (else state)))))
   state))

(define (walk state)
  (if (and (dead-end? state)
	   (< (searched-for state (get-coord)) 15))
      (search state)
      (let* ((paths
	      (list-sort (lambda (a b) (< (length a) (length b)))
			 (filter-map
			  (lambda (c)
			    (find-path-towards state (get-coord) c))
			  (get-state state 'interesting-squares))))
	     (dest (and (not (null? paths))
			(last (car paths)))))
      (if (not dest)
	  (let ((next (new-nu state)))
	    (if next
		(push-action-go state
				(lambda (state)
				  (go-to state next)))
		'done))
	  (move state (map - (cadr (car paths)) (get-coord)))))))

(define (handle-question state)
  (send-expect "\e" expect-generic)
  (display "opting out of question\n")
  state)

(define (new-nu state)
  (define (p? c)
    (and (not (visited? c))
	 (or (any (lambda (c) (not (seen? c)))
		  (neighbor-squares c))
	     (and (dead-end? state c)
		  (< (searched-for state c) 15)))))
  (define (cost state from to)
    (let ((dir (map - to from)))
      (cond ((door? to) 3)
	    ((bad-trap? to) 25)
	    ((and (boulder? to) (not (seen? (map + to dir))))
	     ; explore other areas first if we don't know where
	     ; we're pushing towards
	     15)
	    ((and (char=? (square-char to) #\#)
		  (diagonal? dir))
	     0.7) ; explore corridors first. Shorter.
	    ((and (eq? (car (get-state state 'last-command)) 'move)
		  (equal? dir (cadr (get-state state 'last-command))))
	     0.9) ; prefer to walk in a straight line
	    (else 1))))
  (let ((path (find-path state
			 (get-coord)
			 p?
			 (lambda x 0)
			 (lambda (state from to seed)
			   (passable? state from to))
			 #f
			 cost)))
    (and path (last path))))

(define go-towards
  (let ((path #f)
	(first-dark #f))
    (lambda (state dest)
      (if (not (and path
		    (member (get-coord) path)
		    (equal? (last path) dest)))
	  (let ((new-path (find-path-towards state (get-coord) dest)))
	    (and new-path
		 (begin (set! path new-path)
			(set! first-dark
			      (any (lambda (c)
				     (and (char=? (square-char c) #\space)
					  c))
				   path))
			(go-towards state dest))))
	  (if (and first-dark
		   (not (char=? (square-char first-dark) #\space)))
	      (begin (set! path #f)
		     (go-towards state dest))
	      (let* ((next-square
		      (cadr (find-tail (specialize equal? (get-coord)) path)))
		     (dir (map - next-square (get-coord))))
		(if (not (passable? state (get-coord) next-square))
		    (begin (set! path #f)
			   (go-towards state dest))
		    (move (set-state state
				     'going-towards (and (not (equal? next-square
								      dest))
							 dest))
			  dir))))))))

(define go-to
  (let ((path #f))
    (lambda (state dest)
      (cond ((equal? (get-coord) dest)
	     (pop-action-go state))
	    ((not (and path
		       (member (get-coord) path)
		       (equal? (last path) dest)))
	     (let ((new-path (find-path-to state (get-coord) dest)))
	       (if (not new-path)
		   (pop-action-go (set-state state 'failure 'path-not-found))
		   (begin (set! path new-path)
			  (go-to state dest)))))
	    (else
	     (let* ((next (cadr (find-tail (specialize equal? (get-coord))
					   path))))
	       (cond ((monster? state next)
		      (push-action-go state handle-blocker))
		     ((not (passable? state (get-coord) next))
		      (begin (set! path #f)
			     (go-to state dest)))
		     (else (move state (map - next (get-coord)))))))))))

(define (get-room-extents state)
  (define (extend coord dir)
    (let ((wall-char (if (zero? (car dir)) #\- #\|))
	  (door-char (if (zero? (car dir)) #\| #\-))
	  (from-here (map - coord (get-coord))))
      (let loop ((n (cond ((or (equal? from-here '(0 0))
			       (orthogonal? from-here dir))
			   1)
			  ((equal? from-here dir) 2)
			  (else 0)))
		 (c (map + coord dir))
		 (last coord))
	(cond ((or (and (char=? (square-char c) wall-char)
			(eq? (square-color c) 'black))
		   (and (char=? (square-char c) door-char)
			(eq? (square-color c) 'brown)))
	       n)
	      ((and (or (char=? (square-char c) #\#)
			(and (char=? (square-char c) #\space) (> n 2))) ; problem with partially lit areas
		    (or (char=? (square-char last) #\.)
			(square-clear? state last)))
	       (- n 1))
	      ((and (char=? (square-char c) #\space)
		    (= n 2))
	       #f)
	      ((not (valid-coord? (map + c dir)))
	       #f)
	      (else
	       (loop (+ n 1) (map + c dir) c))))))
  (let* ((directions '((-1 0) (0 -1) (1 0) (0 1)))
	 (starting-points
	  (filter
	   (lambda (c)
	     (and (char=? (square-char c) #\.)
		  (every
		   (lambda (n)
		     (let ((dir (map - n c)))
		       (not (and (not (diagonal? dir))
				 (wall? n)
				 (if (zero? (car (map - n c)))
				     (char=? (square-char n) #\|)
				     (char=? (square-char n) #\-))))))
		   (neighbor-squares c))))
	   (neighbor-squares (get-coord))))
	 (distances
	  (let loop ((start (cons (get-coord) starting-points))
		     (acc '(#f #f #f #f)))
	    (cond ((every identity acc) acc)
		  ((null? start) #f)
		  (else
		   (loop
		    (cdr start)
		    (map (lambda (dis dir)
			   (or dis (extend (car start) dir)))
			 acc
			 directions))))))
	 (displacements
	  (and distances
	       (map (lambda (dis dir)
		      (map (lambda (x) (* x dis)) dir))
		    distances
		    directions))))
    (and displacements
	 ; we combine the displacements for four directions into
	 ; two, north-west and south-east, which we add to our
	 ; starting coordinates to get the coordinates of the
	 ; corners in those two directions.
	 (call-with-values
	     (lambda ()
	       (split-at displacements 2))
	   (lambda (nw se)
	     (list (map + (get-coord) (car nw) (cadr nw))
		   (map + (get-coord) (car se) (cadr se))))))))

(define (print-room-map extents)
  (let* ((nw (car extents)) (se (cadr extents))
	 (x1 (car nw)) (y1 (cadr nw))
	 (x2 (car se)) (y2 (cadr se)))
    (let loop ((y y1))
      (if (<= y y2)
	  (begin
	    (display (get-row-plaintext y x1 x2))
	    (newline)
	    (loop (+ y 1)))))))

(define (corpse-yummy? state mon) #t)

(define (maybe-eat state)
  (let ((objects (get-state state 'objects-here))
	(nutrition (get-state state 'nutrition)))
    (or (and (get-state state 'corpses)
	     objects
	     (not (null? objects))
	     (< nutrition 800)
	     (let ((cutoff (- (turns) 40))
		   (x (assoc (get-coord) (get-state state 'corpses))))
	       (and x
		    (> (caddr x) cutoff)
		    (corpse-yummy? state (cadr x))
		    (any (lambda (item)
			   (let ((mon (item-corpse-of item)))
			     (and mon (string=? (cadr x) mon))))
			 objects)
		    (eat-from-floor state
				    (lambda (item)
				      (let ((c (item-corpse-of item)))
					(and c (string=? c (cadr x)))))))))
	(continue-action state))))

(define (push-action state . ls)
  (set-state state
	     'action
	     (append (reverse ls) (or (get-state state 'action) '()))))

(define (push-action-go state . ls)
  ((last ls)
   (apply push-action state ls)))

(define (pop-action state)
  (set-state state
	     'action
	     (let ((action (get-state state 'action)))
	       (if (or (not action)
		       (null? action))
		   '()
		   (cdr action)))))

(define (pop-action-go state)
  (let* ((s (pop-action state))
	 (f (car (get-state s 'action))))
    (f s)))

(define (continue-action state)
  ((car (get-state state 'action)) state))

(define (handle-blocker state)
  'handle-blocker)
