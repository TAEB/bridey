(define maps
  '(("-------- ------"
     "|<|>...---....|"
     "|^|-.AB....C..|"
     "|^||..DE|.F.G.|"
     "|^||....|.....|"
     "|^|------H----|"
     "|^|    |......|"
     "|^------......|"
     "|..^^^^IJKL...|"
     "|??-----......|"
     "----   --------")
    
    ("------  ----- "
     "|....|  |...| "
     "|.A..----.B.| "
     "|.C......D..| "
     "|..--->---E.| "
     "|---------.---"
     "|..^^^<|.....|"
     "|..----|F....|"
     "--^|   |.G...|"
     " |^-----.H...|"
     " |..^^^^I.J..|"
     " |??----------"
     " ----         ")
    
    (" ----          -----------"
     "--.>--------   |.........|"
     "|..........|   |.........|"
     "|.A-----B-.|   |.........|"
     "|..|...|.C.|   |....<....|"
     "|.D.E....F-|   |.........|"
     "|.G..H..|..|   |.........|"
     "|.----I.--.|   |.........|"
     "|..J...K.|.--  |.........|"
     "|.---L-...M.------------+|"
     "|...|..N-.O.^^^^^^^^^^^^.|"
     "|..P......----------------"
     "-----..|..|               "
     "    -------               ")
    
    ("-----------       -----------"
     "|....|....---     |.........|"
     "|..AB|CD...>|     |.........|"
     "|.....E...---     |.........|"
     "|....|....|       |....<....|"
     "--.---------      |.........|"
     "|..F.|.....|      |.........|"
     "|.GH.|I.J.K|      |.........|"
     "|..L.....M.|      |.........|"
     "|.NOP|Q..R.----------------+-"
     "|....|..S.T.^^^^^^^^^^^^^^^.|"
     "-----------------------------")
    
    ("  --------          "
     "---.|....|          "
     "|...A....|----------"
     "|.-.BC-DE|.|.......|"
     "|.FG-......|.......|"
     "|.-..H.|...|.......|"
     "|....-I--J-|...<...|"
     "|..KL..M...|.......|"
     "|.--...|...|.......|"
     "|....-N|---|.......|"
     "---..O.-----------+|"
     "  |..P>^^^^^^^^^^^.|"
     "  ------------------")
    
    ("--------------------"
     "|........|...|.....|"
     "|.AB..-CD|.-.|.....|"
     "|..|.E.F.|GH.|.....|"
     "|-.|..-..|.-.|..<..|"
     "|...--.......|.....|"
     "|...|.I.-...-|.....|"
     "|.J.|K.|...--|.....|"
     "|-L.|..-----------+|"
     "|..M....^^^^^^^^^^.|"
     "|...|.>-------------"
     "--------")
    
    ("--------------------------"
     "|>......^^^^^^^^^^^^^^^^.|"
     "|.......----------------.|"
     "-------.------         |.|"
     " |...........|         |.|"
     " |.A.B.C.D.E.|         |.|"
     "--------.----|         |.|"
     "|...F.G..H.I.|         |.|"
     "|...J........|         |.|"
     "-----.--------   ------|.|"
     " |..K.L.M...|  --|.....|.|"
     " |.....N....|  |.+.....|.|"
     " |.O.P...Q.--  |-|.....|.|"
     "-------.----   |.+.....+.|"
     "|..R.....|     |-|.....|--"
     "|........|     |.+.....|  "
     "|...------     --|.....|  "
     "-----            -------  ")
    
    ("  ------------------------"
     "  |..^^^^^^^^^^^^^^^^^^..|"
     "  |..-------------------.|"
     "----.|    -----        |.|"
     "|..|A--  --...|        |.|"
     "|.....|--|.B..|        |.|"
     "|.CD..|..|..E.|        |.|"
     "--..FG|...HI.--        |.|"
     " |J..K...|L..|   ------|.|"
     " |.MN.|..|..O| --|.....|.|"
     " |.P.Q---|.R.| |.+.....|.|"
     " |.......|..-- |-|.....|.|"
     " ----.S..|.--  |.+.....+.|"
     "    ---.--.|   |-|.....|--"
     "     |.T...|   |.+.....|  "
     "     |>.|..|   --|.....|  "
     "     -------     -------  ")))

(define solutions
  '((A (5 3)
     B (10 2)
     H (9 7)
     J (8 7)
     plug
     L (10 7)
     plug
     F (11 3))
    (A (3 2)
     C (2 2)
     D (10 3)
     D (3 3)
     E (10 7)
     H (8 9))))

; Same as in pathfinding.scm
(define (make-standard-heuristic goal)
  (lambda (start)
    (min-distance start goal)))
	 
; Walking. Don't push boulders. Pretend square boulder-original has no boulder, and
; square boulder-now has a boulder.
(define (find-path-soko state start goal boulder-original boulder-now)
  (find-path state
	     start
	     (specialize equal? goal)
	     (make-standard-heuristic goal)
	     (lambda (state from to seed)
	       (define (bldr? c)
		 (or (equal? c boulder-now)
		     (and (not (equal? c boulder-original))
			  (boulder? c))))
	       (let ((dir (map - to from)))
		 (not (or (wall? to)
			  (bldr? to)
			  (every (lambda (d)
				   (or (bldr? (map + from d))
				       (wall? (map + from d))))
				 (list (list (car dir) 0)
				       (list 0 (cadr dir))))))))
	     #f
	     (lambda junk 1)))

; the paths monsters can take in soko.
(define (find-path-soko-monster state start goal)
  (find-path state
	     start
	     (specialize equal? goal)
	     (make-standard-heuristic goal)
	     (lambda (state from to seed)
	       (not (or (boulder? to)
			(wall? to)
			(member to (get-state state 'soko-holes)))))
	     #f
	     (lambda junk 1)))
			   

; Boulder transition rules for pathfinder. We save the last vantage point as
; the seed to check if we can get around to push the boulder.
(define boulder-transition
  (let ((boulder #f))
    (lambda (state from to seed)
      (if (eq? (car seed) 'boulder)
	  (begin (set! boulder (cadr seed))
		 (set! seed #f)))
      (let* ((dir (map - to from))
	     (standing (map - from dir))) ; where we push from
	(and (not (or (diagonal? dir)
		      (wall? to)
		      (and (boulder? to)
			   (not (equal? boulder to)))))
	     (find-path-soko state (or seed (get-coord)) standing boulder from)
	     standing)))))

(define (find-path-boulder state start goal)
  (find-path state
	     start
	     (specialize equal? goal)
	     (make-standard-heuristic goal)
	     boulder-transition
	     (list 'boulder start)
	     (lambda junk 1)))

(define (get-layout n)
  (list-ref maps (- n 1)))

(define (get-layout-square n coord)
  (string-ref (list-ref (get-layout n) (cadr coord))
	      (car coord)))

(define (get-layout-dimensions n)
  (let ((layout (get-layout n)))
    (list (- (string-length (car layout)) 1)
	  (- (length layout) 1))))

(define (get-total-extents)
  (iterate-screen
   (lambda (seed coord char color)
     (if (char=? char #\space)
	 seed
	 (apply (lambda (min-coord max-coord)
		  (list (list (min (car coord) (car min-coord))
			      (min (cadr coord) (cadr min-coord)))
			(list (max (car coord) (car max-coord))
			      (max (cadr coord) (cadr max-coord)))))
		seed)))
   '((80 24) (1 1))))

(define (iterate-layout f layout seed)
  (let ((dimensions (get-layout-dimensions layout)))
    (let loop ((c '(0 0))
	       (seed seed))
      (let ((next (if (< (car c) (car dimensions))
		      (map + c '(1 0))
		      (and (< (cadr c) (cadr dimensions))
			   (list 0 (+ (cadr c) 1)))))
	    (seed (f c seed)))
	(if (not next)
	    seed
	    (loop next seed))))))

(define (walls-match? layout offset)
  (call/cc
   (lambda (esc)
     (iterate-layout
      (lambda (c seed)
	(let ((char (get-layout-square layout c)))
	  (or (not (member char '(#\| #\- #\space)))
	      (char=? char (square-char (map + offset c)))
	      (esc #f))))
      layout
      #f))))

(define (find-layout level)
  (let* ((extents (get-total-extents))
	 (offset (car extents))
	 (dimensions (apply (specialize map -) (reverse extents)))
	 (a (- (* level 2) 1))
	 (b (* level 2))
	 (layout (if (equal? dimensions (get-layout-dimensions a))
		     a
		     b)))
    (values (and (walls-match? layout offset)
		 layout)
	    offset)))

(define (get-solution n)
  (list-ref solutions (- n 1)))

(define (in-soko?)
  (call-with-values
    (lambda ()
      (find-layout 1))
    (lambda (layout offset)
      layout)))

(define (parse-starting-boulders layout)
  (iterate-layout
   (lambda (c seed)
     (let ((char (get-layout-square layout c)))
       (if (char-alphabetic? char)
	   (cons (list (string (char-downcase char)) c) seed)
	   seed)))
   layout
   '()))

(define (parse-holes layout)
  (iterate-layout
   (lambda (c seed)
     (if (char=? (get-layout-square layout c) #\^)
	 (cons c seed)
	 seed))
   layout
   '()))

(define (find-closest-hole state)
  (let ((path (find-path
	       state
	       (get-coord)
	       (let ((holes (get-state state 'soko-holes))
		     (offset (get-state state 'soko-offset)))
		 (lambda (c) (member (map - c offset) holes)))
	       (lambda junk 0)
	       (lambda (state from to seed)
		 (not (wall? to)))
	       #f
	       (lambda junk 1))))
    (and path (last path))))

(define (plug state)
  (let ((hole (find-closest-hole state))
	(offset (get-state state 'soko-offset))
	(solution (get-state state 'soko-solution)))
    (if (not hole)
	'done?
	(let* ((boulder
		(any (lambda (b)
		       (and (find-path-boulder state (map + (cadr b) offset) hole)
			    (car b)))
		     (get-state state 'soko-boulders)))
	       (instr (and boulder
			   (list (string->symbol (string-upcase boulder))
				 (map - hole offset)))))
	  (if (not instr)
	      (cond ((null? solution) 'solution-incomplete)
		    ((eq? (car solution) 'plug)
		     (solve-soko (set-state state
					    'soko-solution (cdr solution))))
		    (else 'maybe-the-solution-is-wrong))
	      (solve-soko (set-state state
				     'soko-solution (append instr solution))))))))

; maybe generalize go-to to the point of handling this?
(define next-step
  (let ((path #f))
    (lambda (state dest)
      (if (not (and path
		    (member (get-coord) path)
		    (equal? (last path) dest)))
	  (begin (set! path (find-path-soko state (get-coord) dest #f #f))
		 (next-step state dest))
	  (cadr (find-tail (specialize equal? (get-coord)) path))))))

(define (check-boulders alist offset)
  (every (lambda (e) (char=? (square-char (map + offset (cadr e))) #\0))
	 alist))

(define (solve-soko state)
  (define get (specialize get-state state))
  (define set (specialize set-state state))
  (if (not (get 'soko-layout))
      (call-with-values
	  (lambda () (find-layout 1))
	(lambda (layout offset)
	  (solve-soko (set 'soko-layout layout
			   'soko-offset offset
			   'soko-solution (get-solution layout)
			   'soko-boulders (parse-starting-boulders layout)
			   'soko-holes (parse-holes layout)
			   'soko-waited 0))))
      (let* ((layout (get 'soko-layout))
	     (offset (get 'soko-offset))
	     (boulders (get 'soko-boulders))
	     (solution (get 'soko-solution)))
	(if (or (null? solution)
		(eq? (car solution) 'plug))
	    (if (check-boulders boulders offset)
		(plug state)
		'soko-failed)
	    (let* ((piece (string-downcase (symbol->string (car solution))))
		   (boulder (map + offset (cadr (assoc piece boulders))))
		   (dest (map + offset (cadr solution)))
		   (path (or (and (not (null? (get 'soko-path)))
				  (get 'soko-path))
			     (let ((new (find-path-boulder state boulder dest)))
			       (and new (cdr new)))))
		   (next (and path (not (null? path)) (car path)))
		   (dir (and next (map - next boulder)))
		   (standing (and dir (map - boulder dir))))
	      (cond
	       ((not path)
		'mimic-handling?)
	       ((and (eq? (car (get 'last-command)) 'move)
		     (not (weird-position? state))
		     (equal? boulder (get-coord)))
		(let* ((last-dir (cadr (get 'last-command)))
		       (newpos (map + (get-coord) last-dir (map - offset))))
		  (solve-soko
		   (set 'soko-boulders (assoc-replace (list piece newpos)
						      (get 'soko-boulders))
			'soko-path (cdr path)))))
	       ((equal? boulder dest)
		(if (any (let ((msgs (get 'messages)))
			   (lambda (str) (member str msgs)))
			 '("The boulder fills a pit."
			   "You hear the boulder fall."
			   "The boulder falls into and plugs a hole in the floor!"
			   "Kerplunk!"))
		    (solve-soko
		     (set 'soko-boulders (assoc-delete piece boulders)
			  'soko-holes (delete (map - dest offset)
					      (get 'soko-holes))
			  'soko-solution (cddr (get 'soko-solution))))
		    (solve-soko
		     (set 'soko-solution (cddr (get 'soko-solution))))))
	       ((equal? (get-coord) standing)
		(if (or (char=? (square-char next) #\I)
			(monster? state next))
		    ; try and go kill it
		    (let* ((p (find-path-soko state (get-coord) next #f #f))
			   (sq (and p (cadr p))))
		      (if sq
			  (move state (map - sq (get-coord)))
			  ; try to wait for it. monsters can move diagonally
			  ; between boulders, right?
			  (if (and (find-path-soko-monster state next (get-coord))
				   (< (get 'soko-waited) 12))
			      (wait (set 'soko-waited (+ (get 'soko-waited) 1)))
			      'throw-crap)))
		    (move (set 'soko-path path) dir)))
	       (else
		(let ((sq (next-step state standing)))
		  (move (set 'soko-path path) (map - sq (get-coord)))))))))))
