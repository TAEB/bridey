(define (valid-coord? coord)
  (let ((x (car coord))
	(y (cadr coord)))
    (and (>= x 0) (<= x 80)
	 (>= y 0) (<= y 24))))

(define (monster-string? str) 'todo)

(define (monster? state coord)
  (let ((ch (square-char coord))
	(rogue? (let ((r (get-state state 'rogue-level)))
		  (and r (= r (dlvl))))))
    (or (char-alphabetic? ch)
	(char=? ch #\~) ; might remove
	(member ch '(#\; #\' #\& #\@))
	(and (not rogue?) (char=? ch #\]))
	(and (char=? ch #\:)
	     (or (not rogue?)
		 (monster-string? (far-look coord)))))))

(define (item? state coord)
  ; gold counts but not boulders
  (let ((ch (square-char coord))
	(rogue? (let ((r (get-state state 'rogue-level)))
		  (and r (= r (dlvl))))))
    (or (member ch '(#\! #\$ #\( #\) #\* #\= #\? #\`))
	(and (char=? ch #\")
	     (not rogue?)
	     (eq? (square-color coord) 'blue)) ; XXX check colors
	(and (char=? ch #\_)
	     (eq? (square-color coord) 'cyan))
	(and (char=? ch #\+) ; we'll miss a few books, but whatever
	     (not (door? coord))
	     (or (not (eq? (square-color coord) 'brown))
		 (any (lambda (room) (within-extents? room coord))
		      (get-state state 'rooms))))
	(and rogue?
	     (or (char=? ch #\,)
		 (char=? ch #\])
		 (and (char=? ch #\:)
		      (not (monster-string? (far-look coord))))))
	(and (not rogue?)
	     (or (char=? ch #\%)
		 (char=? ch #\[))))))

(define (fountain? state coord)
  (or (and (char=? (square-char coord) #\{)
	   (eq? (square-color coord) 'blue))
      (member (list (dlvl) coord) (get-state state 'fountains))))

(define (throne? state coord)
  (or (and (char=? (square-char coord) #\\)
	   (eq? (square-color coord) 'yellow))
      (member (list (dlvl) coord) (get-state state 'thrones))))

(define (dung-feature? state coord)
  (or (trap? coord)
      (fountain? state coord)
      (throne? state coord)
      ;(altar? state coord)
      (char=? (square-char coord) #\{)
      (char=? (square-char coord) #\\)))

(define (trap-type state coord)
  (and (trap? coord)
       (let ((e (assoc coord (get-state state 'traps))))
	 (if (not e)
	     (display "trap-type: missing trap\n")
	     (cadr e)))))

; incomplete..?
(define (square-clear? state coord)
  (case (square-char coord)
    ((#\. #\{ #\^ #\{#\{#\\) #t)
    ((#\# #\_ #\") (eq? (square-color coord) 'black))
    (else #f)))

(define (engulfed?)
  (define (all-same? ls)
    (or (null? ls)
	(null? (cdr ls))
	(and (eq? (car ls) (cadr ls))
	     (all-same? (cdr ls)))))
  (let ((n (neighbor-squares)))
    (and (equal? '(#\/ #\- #\\
		   #\|     #\|
		   #\\ #\- #\/)
		 (map square-char n))
	 (all-same? (map square-color n)))))

(define (within-extents? ext . opt) ; can't be on the line
  (let* ((nw (car ext)) (ne (cadr ext))
	 (coord (if (null? opt) (get-coord) (car opt))))
    (and (every positive? (map - coord nw))
	 (every negative? (map - coord ne)))))

(define (dir->vi dir)
  (case (cadr dir)
    ((-1) (case (car dir) ((-1) "y") ((0) "k") ((1) "u")))
    (( 0) (case (car dir) ((-1) "h") ((0)  #f) ((1) "l")))
    (( 1) (case (car dir) ((-1) "b") ((0) "j") ((1) "n")))))

(define (vi->dir vi)
  (case (string-ref vi 0)
    ((#\y) '(-1 -1)) ((#\k) '(0 -1)) ((#\u) '(1 -1))
    ((#\h) '(-1  0))                 ((#\l) '(1  0))
    ((#\b) '(-1  1)) ((#\j) '(0  1)) ((#\n) '(1  1))))

;; (define (nsteps-to xy . opt)
;;   (let* ((pos (if (null? opt) (get-coord) (car opt)))
;; 	 (dir (step-toward xy pos)))
;;     (if (equal? dir '(0 0))
;; 	0
;; 	(+ 1 (nsteps-to xy (map + pos dir))))))

;(define (eight-dirs)
;  (map vi->dir '(h y k u l n j b)))

(define neighbor-squares
  (let ((dirs '((-1 -1) ( 0 -1) ( 1 -1)
		(-1  0)         ( 1  0)
		(-1  1) ( 0  1) ( 1  1))))
    (lambda opt
      (let ((coord (if (null? opt) (get-coord) (car opt))))
	(filter
	 (lambda (c)
	   (and (>= (car c) 1) (<= (car c) 80)
		(>= (cadr c) 1) (<= (cadr c) 24)))
	 (map (specialize map + coord) dirs))))))

(define (diagonal? dir)
  (not (= (abs (apply + dir)) 1)))

(define (weird-position? state)
  (not (equal? (get-coord) (get-state state 'expected-coord))))

(define (boulder? coord)
  (char=? (square-char coord) #\0))

; TODO
(define (bad-trap? coord) #f)

;(define (touching? a b)
;  (= 1 (nsteps-to a b)))

; this is kind of bad
(define (dead-end? state . opt)
  (let ((coord (if (null? opt) (get-coord) (car opt))))
    (= 3 (count
	  (lambda (c)
	    (and (not (diagonal? (map - c coord)))
		 (or (char=? (square-char c) #\space)
		     (wall? c))))
	  (neighbor-squares coord)))))

(define (passable? state from to)
  (let ((dir (map - to from)))
    (not (or (wall? to)
	     (boulder? to)
	     (char=? (square-char to) #\space)
	     (and (boulder? to)
		  (valid-coord? (map + to dir))
		  (or (wall? (map + to dir))
		      (member (list to dir)
			      (get-state state 'stuck-boulders))))
	     (and (or (door? to)
		      (door? from))
		  (diagonal? dir))
	     (and (embedded? to)
		  (not (door? to)))))))

(define (adjacent? a b)
  (<= (apply max (map (compose abs -) a b)) 1))

(define (min-distance a b)
  (max (abs (- (car a) (car b)))
       (abs (- (cadr a) (cadr b)))))

; Square information. Visited squares, doors, traps, etc. The mark/unmark stuff.
; These are in a bitfield because pathfinding uses them so they need to be fast.
(define square-info #f)
(define square-info-trap-types '())

(define (square-info-init) (set! square-info (make-byte-vector (* 80 24) 0)))
(define (square-info-load-level) 'todo)

(define (square-info-set bit coord)
  (let* ((i (coord->i coord))
	 (old (byte-vector-ref square-info i)))
    (byte-vector-set! square-info i (set-bit old bit))))

(define (square-info-unset bit coord)
  (let* ((i (coord->i coord))
	 (old (byte-vector-ref square-info i)))
    (byte-vector-set! square-info i (unset-bit old bit))))

(define (square-info-set? bit coord)
  (bit-set? (map-bv-ref square-info coord) bit))

(define (visited? c) (square-info-set? 0 c))
(define (mark-visited c) (square-info-set 0 c))

; generalize bitfields for multibit? hrm. whatever.
(define (door? c)
  (or (square-info-set? 1 c)
      (square-info-set? 2 c)
      ; unambiguously identify cleared open doors from map
      (and (eq? (square-color c) 'brown)
	   (memv (square-char c) '(#\| #\-))
	   (begin (mark-open-door c) #t))))
(define (unmark-door c)
  (square-info-unset 1 c)
  (square-info-unset 2 c))
(define (open-door? c)
  (and (square-info-set? 1 c)
       (not (square-info-set? 2 c))))
(define (mark-open-door c)
  (square-info-set 1 c)
  (square-info-unset 2 c))
(define (closed-door? c)
  (and (not (square-info-set? 1 c))
       (square-info-set? 2 c)))
(define (mark-closed-door c)
  (square-info-unset 1 c)
  (square-info-set 2 c))
(define (locked-door? c)
  (and (square-info-set? 1 c)
       (square-info-set? 2 c)))
(define (mark-locked-door c)
  (square-info-set 1 c)
  (square-info-set 2 c))

(define (seen? c) (square-info-set? 3 c))
(define (mark-seen c) (square-info-set 3 c))

(define (trap? c) (square-info-set? 5 c))
(define (mark-trap coord type)
  (square-info-set 5 coord)
  (set! square-info-trap-types
	(assoc-replace (cons coord type) square-info-trap-types)))
(define (unmark-trap c) (square-info-unset 5 c))
(define (trap-type coord)
  (and (trap? coord)
       (assoc coord square-info-trap-types)))

(define (embedded? c) (square-info-set? 6 c))
(define (mark-embedded c) (square-info-set 6 c))
(define (unmark-embedded c) (square-info-unset 6 c))

(define (wall? coord)
  (and (eq? (square-color coord) 'black)
       (or (char=? (square-char coord) #\|)
	   (char=? (square-char coord) #\-))))

(define (searched-for state coord)
  (map-bv-ref (get-state state 'searched) coord))

(define (mark-all-corridors-seen)
  (iterate-screen
   (lambda (seed coord char color)
     (if (char=? char #\#)
	 (for-each mark-seen (neighbor-squares coord))))
   #f
   '(1 2)
   '(80 22)))
	  

; no diagonals
(define (orthogonal? dir1 dir2)
  (or (and (zero? (car dir1)) (zero? (cadr dir2)))
      (and (zero? (cadr dir1)) (zero? (car dir2)))))

(define (same-level? state)
  'intra-level-support-coming-soon)

