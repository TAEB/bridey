(define (initial-state)
  (let ((state
	 (set-state
	  '()
	  'explorer identity
	  'coord (get-coord)
	  'last-coord (get-coord)
	  'expected-coord (get-coord)
	  'last-command '(#f #f)
	  'inventory '()
	  'avatar #\@ ; TODO: char selection. Right now we're all bar-hum-mal-cha
	  'searched (make-byte-vector (* 80 24) 0)
	  'blind? #f
	  'stuck-boulders '()
	  'rooms '()
	  'searched-walls '()
	  'fountains '()
	  'thrones '()
	  'pickup-pred (lambda (x) #f))))
    (mark-visited (get-coord))
    (update-seen-squares state)
    state))

(define (process-turn state)
  ; need a macro for this :)
  (let* ((state (read-topl (set-state state 'messages '())))
	 (state (set-state state 'coord (get-coord)))
	 (state (fold process-message state (get-state state 'messages)))
	 (get (specialize get-state state))
	 (set (specialize set-state state))
	 (coord (get-coord)))
    (botl-update)
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
	    (else ((get 'explorer) state))))))

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


(define (update-seen-squares state)
  (iterate-screen
   (lambda (seed coord char color)
     (if (not (or (seen? coord)
		  (char=? char #\space)
		  (char=? char #\I)))
	 (mark-seen coord)))
   #f
   '(1 2) '(80 22)))

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
     (if (or (char=? char #\space)
	     (char=? char #\I))
	 state
	 (let ((seen (seen? coord)))
	   (if (not seen) (mark-seen coord))
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
	    ((and (door? coord)
		  (square-clear? state coord))
	     (unmark-door coord)
	     state)
	    ((and (not (visited? coord))
		  (item? state coord))
	     (cons-state state 'interesting-squares coord))
	    (else state)))))
   state
   '(1 2) '(80 22)))

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
			(lambda (c)
			  (find-path-towards state (get-coord) c))
			(get-state state 'interesting-squares)))))
      (if (null? paths)
	  (let ((next (nu state)))
	    (if next
		(go-to state next)
		'done))
	  (go-to state (last (car paths))))))))

(define (handle-question state)
  (send-expect "\e" expect-generic)
  (display "opting out of question\n")
  state)

(define (read-topl state)
  (if (at-question?)
      (read-topl (handle-question state))
      (let ((state (set-state
		    state
		    'messages (append (reverse (read-messages))
				      (get-state state 'messages)))))
	(if (at-more?)
	    (begin (send-expect " " expect-generic)
		   (read-topl state))
	    state))))


(define (nu state)
  (define node-coord car)
  (define node-cost cadr)
  (define (cost to from)
    (let ((dir (map - to from)))
      (cond ((door? to) 3)
	    ((bad-trap? state to) 25)
	    ((and (boulder? to) (not (seen? (map + to dir))))
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
	   (if (and (not (visited? current))
		    (or (any (lambda (c) (not (seen? c))) neighbors)
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

(define go-towards
  (let ((path #f)
	(first-dark #f))
    (lambda (state dest)
      (if (not (and path
		    (member (get-coord) path)
		    (equal? (last path) dest)))
	  (let ((new-path (find-path-towards state (get-coord) dest)))
	    (if (not new-path)
		'path-not-found
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
      (if (not (and path
		    (member (get-coord) path)
		    (equal? (last path) dest)))
	  (let ((new-path (find-path-to state (get-coord) dest)))
	    (if (not new-path)
		'path-not-found
		(begin (set! path new-path)
		       (go-to state dest))))
	  (let* ((next-square
		  (cadr (find-tail (specialize equal? (get-coord)) path)))
		 (dir (map - next-square (get-coord))))
	    (if (not (passable? state (get-coord) next-square))
		(begin (set! path #f)
		       (go-to state dest))
		(move (set-state state
				 'going-to (and (not (equal? next-square dest))
						dest))
		      dir)))))))

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


