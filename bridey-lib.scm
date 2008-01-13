(define (initial-state)
  (ask-connect)
  (term-init)
  (term-process (get-raw-output))
  (let ((state
	 (set-state
	  '()
	  'explorer identity
	  'last-coord (get-coord)
	  'expected-coord (get-coord)
	  'last-command '(#f #f)
	  'inventory '()
	  'visited (make-byte-vector (* 80 24) 0)
;	  'splits '()
;	  'path '()
;	  'searched-for 0
	  'searched (make-byte-vector (* 80 24) 0)
;	  'in-doorway? #f
	  'stuck-boulders '()
;	  'push-boulder? #f
	  'rooms '()
;	  'seen-corridors '()
	  'searched-walls '()
;	  'nearby-doors '()
	  'fountains '()
	  'thrones '()
	  'pickup-pred (lambda (x) #f))))
    (mark-visited state (get-coord))
    (update-seen-squares state)
;    (if (not (blind?))
;	(for-each (specialize mark-seen state)
;		  (neighbor-squares)))
    state))

; This is the main dispatcher; functions that directly cause a turn
; to end call this.
(define (process-turn state)
  (let ((state
	 (fold process-messages
	       (set-state state
			  'coord (get-coord)
			  'messages '())
	       (get-msgs))))
    (term-process (get-raw-output))
    (botl-update)
    (mark-visited state (get-coord))
    (mark-seen state (get-coord))
    (let ((squares-with-mons
	   (filter (compose hostile? cadr far-look)
		   (filter (specialize monster? state)
			   (neighbor-squares))))
	  (search (get-state state 'searching-for)))
      (cond ((not (null? squares-with-mons)) (hit-something state))
	    (else ((get-state state 'explorer) state))))))

(define (hit-something state)
  (let ((squares-with-mons
	 (filter (compose hostile? cadr far-look)
		 (filter (specialize monster? state)
			 (neighbor-squares)))))
    (fight state (step-toward (car squares-with-mons)))))

(define (want-item? item)
  (string=? (item-name item) "food ration"))

(define (do-door state dir)
  (define (c=? c) (char=? (square-char-dir dir) c))
  (define (wall-in-dir? dir) (wall? (map + (get-coord) dir)))
  (define (get-aligned)
    (let* ((dir1 (list (car dir) 0))
	   (dir2 (list (cadr dir) 0))
	   (sqr1 (map + (get-coord) dir1))
	   (sqr2 (map + (get-coord) dir2))
	   (ls (list sqr1 sqr2))
	   (bad? (lambda (c) (or (wall? c) (bad-trap? state c)))))
      (if (not (bad? sqr1))
	  (move state dir1)
	  (and (not (bad? sqr2))
	       (move state dir2)))))
  (let ((coord (map + (get-coord) dir)))
    (if (or (c=? #\.)
	    (and (not (and (c=? #\+) (eq? (square-color coord) 'brown)))
	       (not (diagonal? dir))
	       (not (embedded? state coord))))
	(move state dir)
	(if (open-door dir)
	    (begin (mark-door state coord)
		   (unmark-embedded state coord)
		   (process-turn
		    (set-state state
			       'last-command (list 'open-door dir)
			       'expected-coord (get-state state 'coord))))
	    (cond ((member "This door is locked." (get-msgs))
		   (mark-door state coord)
		   (if #f ; have-key
		       'unlock
		       (if (diagonal? dir)
			   (get-aligned)
			   (if (> (nchars-identical (engraving)
						    "Closed for inventory.")
				  4)
			       'make-a-note-somewhere
			       (kick state dir)))))
		  ((or (member "This doorway has no door." (get-msgs))
		       (member "This door is broken." (get-msgs))
		       (member "You see no door there." (get-msgs)))
		   (unmark-door state coord)
		   (move state dir #t))
		  ((member "This door is already open." (get-msgs))
		   (mark-door state coord)
		   (if (diagonal? dir)
		       (get-aligned)
		       (move state dir)))
		  ((member "The door resists!" (get-msgs))
		   (mark-door state coord)
		   (process-turn
		    (set-state state
			       'last-command (list 'open-door dir)
			       'expected-coord (get-state state 'coord))))
		  (else 'bad-case))))))
  

(define (old-do-door state dir)
  (define (c=? c) (char=? (square-char-dir dir) c))
  (define (wall-in-dir? dir) (wall? (map + (get-coord) dir)))
  (define (enter-doorway)
    (move (set-state state 'in-doorway? (map + (get-coord) dir)) dir))
  (define (mark) (mark-door state (map + (get-coord) dir)))
  (define (get-aligned)
    (let ((wall-dir
	   (cond ((c=? #\-) (map * '(1 0) dir))
		 ((c=? #\|) (map * '(0 1) dir))
		 ((wall-in-dir? (map * dir '(1 0))) (map * '(1 0) dir))
		 (else (map * '(0 1) dir)))))
      ; Example: say we have an open door to our north-west (-1 -1) and we
      ; figure out from the door character (|, as opposed to -) that the wall
      ; is to our due north (0 -1).  We reverse north to get 0 in the y (-1 0),
      ; then map-multiply the abs of the resulting vector on dir in order to
      ; eliminate the vertical portion, leaving us with west (-1 0), which is
      ; the direction we want to move to align ourselves with the door.
      (move state (map * dir (map abs (reverse wall-dir))))))
  (if (or (c=? #\.)
	  (and (not (and (c=? #\+)
			 (eq? (square-color-dir dir) 'brown)))
	       (not (diagonal? dir))))
      (enter-doorway)
      (if (open-door dir)
	  (begin (mark)
		 (process-turn
		  (set-state state
			     'expected-coord (get-state state 'coord))))
	  (cond ((member "This door is locked." (get-msgs))
		 (mark)
		 (if #f ; have-key
		     'unlock
		     (if (diagonal? dir)
			 (get-aligned)
			 (if (> (nchars-identical (engraving)
						  "Closed for inventory.")
				4)
			     'make-a-note-somewhere
			     (kick state dir)))))
		((or (member "This doorway has no door." (get-msgs))
		     (member "This door is broken." (get-msgs))
		     (member "You see no door there." (get-msgs)))
		 (enter-doorway))
		((member "This door is already open." (get-msgs))
		 (mark)
		 (if (diagonal? dir)
		     (get-aligned)
		     (enter-doorway)))
		((member "The door resists!" (get-msgs))
		 (mark)
		 (process-turn
		  (set-state state
			     'expected-coord (get-state state 'coord))))
		(else 'bad-case)))))

(define (update-seen-squares state)
  (iterate-screen
   (lambda (seed coord char color)
     (if (not (or (seen? state coord)
		  (char=? char #\space)
		  (char=? char #\I)))
	 (mark-seen state coord)))
   #f))

(define (draw-seen state)
  (do ((y 1 (+ y 1)))
      ((= y 24))
    (do ((x 1 (+ x 1)))
	((= x 80))
      (display (if (seen? state (list x y)) "#" ".")))
    (newline)))

(define (update-map state)
  (iterate-screen
   (lambda (state coord char color)
     (if (or (char=? char #\space)
	     (char=? char #\I))
	 state
	 (let ((seen? (seen? state coord)))
	   (if (not seen?) (mark-seen state coord))
	   (cond
	    ((and (eq? color 'blue)
		  (char=? char #\{))
	     (modify-state
	      state
	      'fountains (lambda (ls)
			   (let ((entry (list (dlvl) coord)))
			     (if (member entry ls)
				 ls
				 (cons entry ls))))))
	    ((and (door? state coord)
		  (square-clear? state coord))
	     (unmark-door state coord)
	     state)
	    ((and (not (visited? state coord))
		  (item? state coord))
	     (cons-state state 'interesting-squares coord))
	    (else state)))))
   state))

(define (walk state)
  (cond
   ((get-state state 'going-to)
    (go-to (set-state state 'explorer walk)
	   (get-state state 'going-to)))
   ((and (dead-end? state)
	 (< (searched-for state (get-coord)) 15))
    (search (set-state state 'explorer walk)))
   (else
;;     (let* ((interesting-stuff
;; 	    (iterate-screen
;; 	     (lambda (seed coord char color)
;; 	       (if (not (or (seen? state coord)
;; 			    (char=? char #\space)
;; 			    (char=? char #\I)))
;; 		   (mark-seen state coord))
;; 	       (if (and (not (char=? char #\space))
;; 			(not (visited? state coord))
;; 			(item? state coord))
;; 		   (cons coord seed)
;; 		   seed))
;; 	     '()))
    (let* ((state (update-map
		   (set-state
		    state
		    'explorer walk
		    'interesting-squares '())))
	   (paths
	    (list-sort (lambda (a b) (< (length a) (length b)))
		       (filter-map
			(specialize a-star state (get-coord))
			(get-state state 'interesting-squares)))))
      (if (null? paths)
	  (let ((next (nu state)))
	    (if next
		(go-to state next)
		'done))
	  (go-to state (last (car paths))))))))


(define (nu state)
  (define node-coord car)
  (define node-cost cadr)
  (define (cost to from)
    (let ((dir (map - to from)))
      (cond ((door? state to) 3)
	    ((bad-trap? state to) 25)
	    ((and (boulder? to) (not (seen? state (map + to dir))))
	     ; explore other areas first if we don't know where we're pushing
	     ; towards
	     15)
	    (else 1))))
  (define (make-node c p)
    (list c (if p
		(+ (node-cost p) (cost c (node-coord p)))
		0)))
  (let loop ((open (list (make-node (get-coord) #f)))
	     (closed '()))
    (and (not (null? open))
	 (let* ((current (node-coord (car open)))
		(neighbors (neighbor-squares current)))
	   (display "nu: examining ")
	   (write (car open))
	   (newline)
	   (if (and (not (visited? state current))
		    (or (any (lambda (c) (not (seen? state c))) neighbors)
			(and (dead-end? state current)
			     (< (searched-for state current) 15))))
	       current
	       (loop (list-merge
		      (lambda (a b) (< (node-cost a) (node-cost b)))
		      (cdr open)
		      (filter-map
		       (lambda (c)
			 (and (not (member c closed))
			      (passable? state c current)
			      (let ((node (make-node c (car open)))
				    (old (assoc c open)))
				(and (or (not old)
					 (< (node-cost node) (node-cost old)))
				     node))))
		       
		       neighbors))
		     (cons current closed)))))))

(define go-to
  (let ((path #f))
    (lambda (state dest)
      (if (not (and path
		    (member (get-coord) path)
		    (equal? (last path) dest)))
	  (let ((new-path (a-star state (get-coord) dest)))
	    (if (not new-path)
		'path-not-found
		(begin (set! path new-path)
		       (go-to state dest))))
	  (let* ((next-square
		  (cadr (find-tail (specialize equal? (get-coord)) path)))
		 (dir (map - next-square (get-coord))))
	    (if (not (passable? state next-square (get-coord)))
		(begin (set! path #f)
		       (go-to state dest))
		(move (set-state state
				 'going-to (and (not (equal? next-square dest))
						dest))
		      dir)))))))

(define (a-star state start goal)
  (define (make-node parent sq)
    (let ((g-val (if (not parent) 0 (+ 1 (g parent))))
	  (h-val (let ((dx (abs (- (car goal) (car sq))))
		       (dy (abs (- (cadr goal) (cadr sq)))))
		   (max dx dy))))
      (list sq parent (+ g-val h-val) g-val h-val)))
  (define (make-start-node sq)
    (make-node #f sq))
  (define (coord node) (list-ref node 0))
  (define (parent node) (list-ref node 1))
  (define (f node) (list-ref node 2))
  (define (g node) (list-ref node 3))
  (define (h node) (list-ref node 4))

  (define (path-to node)
    (define (acc n)
      (if (not (parent n))
	  (list (coord n))
	  (cons (coord n) (acc (parent n)))))
    (reverse (acc node)))
  
  (let loop ((open (list (make-start-node start)))
	     (closed '()))
    (cond
     ((null? open) #f)
     ((equal? (coord (car open)) goal) (path-to (car open)))
     (else
;      (display "examining ")
;      (write (car open))
;      (newline)
      (let* ((current (car open))
	     (neighbors
	      (map (specialize make-node current)
		   (filter
		    (lambda (s)
		      (and (not (member s (map coord closed)))
			   (passable? state s (coord current))))
		    (neighbor-squares (coord current)))))
	     (to-add
	      (filter
	       (lambda (node)
		 (or (not (member (coord node) (map coord open)))
		     (< (g node)
			(g (first (lambda (n)
				    (equal? (coord n) (coord node)))
				  open)))))
	       neighbors)))
	(loop
	 (list-merge (lambda (a b) (< (f a) (f b)))
		     (filter (lambda (node)
			       (not (member (coord node) (map coord to-add))))
			     (cdr open))
		     to-add)
	 (cons current closed)))))))

; this is just... yuck.  I'll try to think of something better.
;; (define (search-wall state ext wall-dir)
;;   (define get (specialize get-state state))
;;   (define set (specialize set-state state))
;;   (define (distance a b) (abs (apply + (map - a b))))
;;   (define (done) (do-room (set 'search-wall-in-progress? #f
;; 			       'search-wall-line #f
;; 			       'searched-walls (cons (list ext wall-dir)
;; 						     (get 'searched-walls))
;; 			       'extents ext)))
;;   (if (not (get 'search-wall-in-progress?))
;;       (let* ((nw (car ext)) (se (cadr ext))
;; 	     (x1 (+ (car nw) 1)) (y1 (+ (cadr nw) 1))
;; 	     (x2 (- (car se) 1)) (y2 (- (cadr se) 1))
;; 	     (coords
;; 	      (let ((x (list (list (if (= (car wall-dir) 1) x2 x1)
;; 				   (if (= (cadr wall-dir) 1) y2 y1))
;; 			     (list (if (= (car wall-dir) -1) x1 x2)
;; 				   (if (= (cadr wall-dir) -1) y1 y2)))))
;; 		(if (< (nsteps-to (car x)) (nsteps-to (cadr x)))
;; 		    x
;; 		    (reverse x))))
;; 	     (from (car coords))
;; 	     (to (cadr coords))
;; 	     (dir (step-toward to from))
;; 	     (line (drop-while
;; 		    (specialize bad-trap? state)
;; 		    (map (lambda (x)
;; 			   (map + from (map (specialize * x) dir)))
;; 			 (iota (+ (distance to from) 1))))))
;; 	(if (null? line)
;; 	    (done)
;; 	    (let ((st (set 'search-wall-in-progress? #t
;; 			   'search-wall-line line)))
;; 	      (if (equal? (get-coord) (car line))
;; 		  (search-wall st ext wall-dir)
;; 		  (go-to st (car line))))))
;;       (if (< (get 'searched-for) 15)
;; 	  (search state)
;; 	  (let ((line (get 'search-wall-line)))
;; 	    (if (< (length line) 3)
;; 		(done)
;; 		(let* ((line (get 'search-wall-line))
;; 		       (next
;; 			(or (first (lambda (s) (not (bad-trap? state s)))
;; 				   (reverse (take line 3)))
;; 			    (first (lambda (s) (not (bad-trap? state s)))
;; 				   (drop line 3)))))
;; 		  (if (not next)
;; 		      (done)
;; 		      (go-to (set 'search-wall-in-progress? #t
;; 				  'search-wall-line
;; 				  (cdr (find-tail (specialize equal? next)
;; 						  line)))
;; 			     next))))))))


  


    
