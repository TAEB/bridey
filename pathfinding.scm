(define (standard-transition state from to seed)
  (passable? state from to))

(define (make-standard-heuristic goal)
  (lambda (start)
    (min-distance start goal)))

(define (standard-cost state from to)
  (let ((dir (map - to from)))
    (cond ((door? to) 3)
	  ((bad-trap? to) 30)
	  ((and (boulder? to)
		(valid-coord? (map + to dir))
		(not (seen? (map + to dir))))
	   ; explore other areas first if we don't know where we're pushing
	   ; towards
	   15)
	  (else 1))))

(define (find-path-to state start goal)
  (find-path state
	     start
	     (specialize equal? goal)
	     (make-standard-heuristic goal)
	     standard-transition
	     #f
	     standard-cost))

; include dark squares
(define (find-path-towards state start goal)
  (find-path state
	     start
	     (specialize equal? goal)
	     (make-standard-heuristic goal)
	     (lambda (state from to seed)
	       (or (passable? state from to)
		   (and (not (and (door? from)
				  (diagonal? (map - to from))))
			(not (seen? to))
			(char=? (square-char to) #\space))))
	     #f
	     (lambda (state from to)
	       (if (char=? (square-char to) #\space)
		   1.5
		   (standard-cost state from to)))))

(define (find-path state start pred heuristic transition transition-seed cost)
  (define (make-node parent seed sq)
    (let ((g-val (if parent (+ (g parent) (cost state (coord parent) sq)) 0))
	  (h-val (heuristic sq)))
      (list sq parent seed (+ g-val h-val) g-val h-val)))
  (define (make-start-node sq)
    (make-node #f transition-seed sq))
  (define (coord node) (list-ref node 0))
  (define (parent node) (list-ref node 1))
  (define (seed node) (list-ref node 2))
  (define (f node) (list-ref node 3))
  (define (g node) (list-ref node 4))
  (define (h node) (list-ref node 5))

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
     ((pred (coord (car open)))
      (path-to (car open)))
     (else
;      (display "examining ")
;      (write (car open))
;      (newline)
      (let* ((current (car open))
	     (neighbors
	      (let loop ((ls '())
			 (squares (neighbor-squares (coord current))))
		(cond
		 ((null? squares) ls)
		 ((member (car squares) closed)
		  (loop ls (cdr squares)))
		 (else
		  (let ((s (transition state
				       (coord current)
				       (car squares)
				       (seed current))))
		    (if s
			(loop (cons (make-node current s (car squares))
				    ls)
			      (cdr squares))
			(loop ls (cdr squares))))))))
	     (to-add
	      (filter
	       (lambda (node)
		 (or (not (member (coord node) (map coord open)))
		     (< (g node)
			(g (any (lambda (n)
				  (and (equal? (coord n) (coord node))
				       n))
				open)))))
	       neighbors)))
	(loop
	 (list-merge (lambda (a b) (< (f a) (f b)))
		     (filter (lambda (node)
			       (not (member (coord node) (map coord to-add))))
			     (cdr open))
		     to-add)
	 (cons (coord current) closed)))))))
