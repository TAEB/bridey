(define (valid-coord? coord)
  (apply (lambda (x y)
	   (and (>= x 0) (<= x 80)
		(>= y 2) (<= y 22)))
	 coord))

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
		 (monster-valid? (far-look coord)))))))

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
		      (not (monster-valid? (far-look coord))))))
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
    ((#\# #\_ #\") (eq? (square-color coord) 'none))
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
    (( 1) (case (car dir) ((-1) "b") ((0) "j") ((1) "n")))
    (else 'bad-dir-in-dir->vi)))

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
  (or (get-state state 'weird-position?)
      (let ((exp (get-state state 'expected-coord)))
	(and exp
	     (not (equal? (get-coord) exp))))))

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
    (not (or (char=? (square-char to) #\space)
	     (wall? to)
	     (and (boulder? to)
		  (or (member (list to dir) (get-state state 'stuck-boulders))
		      (let ((beyond (map + to dir)))
			(and (valid-coord? beyond)
			     (seen? beyond)
			     (or (char=? (square-char beyond) #\space)
				 (wall? beyond))))))
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

(define square-covered-list '())
(define (square-covered-by-item? c) (square-info-set? 4 c))
(define (set-square-covered-by c)
  (square-info-set 4 c)
  (set! square-covered-list
	(assoc-replace (list c (square-char c) (square-color c))
		       square-covered-list)))
(define (square-covered-match-current? c)
  (and (square-covered-by-item? c) ; don't call on empty square
       (let* ((x (cdr (assoc c square-covered-list)))
	      (char (car x))
	      (color (cadr x)))
	 (and (char=? char (square-char c))
	      (eq? color (square-color c))))))

(define (unmark-square-covered-by-item c) (square-info-unset 4 c))

(define (trap? c) (square-info-set? 5 c))
(define (mark-trap coord type)
  (square-info-set 5 coord)
  (set! square-info-trap-types
	(assoc-replace (cons coord type) square-info-trap-types)))
(define (unmark-trap c) (square-info-unset 5 c))
(define (trap-type coord)
  (and (trap? coord)
       (cdr (assoc coord square-info-trap-types))))

(define (embedded? c) (square-info-set? 6 c))
(define (mark-embedded c) (square-info-set 6 c))
(define (unmark-embedded c) (square-info-unset 6 c))

(define (wall? coord)
  (member (square-glyph coord)
	  '((none #\|) (none #\-))))

(define (searched-for state coord)
  (map-bv-ref (get-state state 'searched) coord))

(define (mark-all-corridors-seen)
  (iterate-screen
   (lambda (seed coord glyph)
     (if (equal? glyph '(none #\#))
	 (for-each mark-seen (neighbor-squares coord))))
   #f))

; no diagonals
(define (orthogonal? dir1 dir2)
  (or (and (zero? (car dir1)) (zero? (cadr dir2)))
      (and (zero? (cadr dir1)) (zero? (car dir2)))))

(define (same-level? state)
  'inter-level-support-coming-soon)

(define (maybe-add-corpse state monster)
  (let* ((cmd (get-state state 'last-command))
	 (coord (and (eq? (car cmd) 'fight)
		     (map + (get-state state 'expected-coord) (cadr cmd)))))
    (if (and coord
	     (monster-leaves-corpse? monster)
	     (not (square-covered-match-current? coord))
	     (char=? (square-char coord) #\%)
	     (eq? (square-color coord) (monster-color monster)))
	(let ((corpses (or (get-state state 'corpses) '()))
	      (cutoff (- (turns) 40)))
	  (set-state state
		     'corpses
		     (cons (list coord monster (turns))
			   (remove (lambda (e)
				     (or (< (caddr e) cutoff)
					 (and (equal? coord (car e))
					      (string=? monster (cadr e)))))
				   corpses))))
	state)))

(define (add-fountain state coord)
  (modify-state
   state
   'fountains (lambda (ls)
		(let ((x (list (dlvl) coord)))
		  (if (member x ls)
		      ls
		      (cons x ls))))))

(define (remove-fountain state coord)
  (modify-state
   state
   'fountains (lambda (ls)
		(delete (list (dlvl) coord ) ls))))

(define (decrement-item state slot)
  (let* ((inventory (get-state state 'inventory))
	 (item ((cadr (assoc slot inventory))))
	 (n (item-quantity item)))
    (if (<= (item-quantity item) 1)
	(set-state state 'do-inventory? #t)
	(set-state
	 state
	 'inventory
	 (assoc-replace (list slot (item-adjust-quantity item (- n 1))))))))

(define (alignment->number align)
  (case align
    ((lawful) 1)
    ((neutral) 0)
    ((chaotic) -1)
    (else #f)))

(define (create-level state)
  (set-state
   state
   'searched (make-byte-vector (* 80 24) 0)
   'stuck-boulders '()
   'rooms '()
   'searched-walls '()
   'branch 'doom
   'interesting-squares '()))

(define (load-level state branch lvl)
  (define (get-level state branch lvl)
    (vector-ref (get-state state 'level-info)
		(+ lvl
		   (case branch
		     ((doom) 0) ; 0-53
		     ((mines) 54) ; 54-63
		     ((soko) 64) ; 64-67
		     ((home) 68) ; 68-73
		     ((ludios) 74) ; 74
		     ((vlad) 75) ; 75-77
		     ((planes) 76) ; 76-80
		     ((unknown) 81))))) ; 81-90
  (define (copy-state from to ls)
    (fold (lambda (name new)
	    (set-state new name (get-state from name)))
	  to
	  ls))
  (let ((old (get-level state
			(get-state state 'branch)
			(or (dlvl) (get-state state 'dlvl))))
	(new (get-level state branch lvl))
	(states '(branch searched stuck-boulders searched-walls rooms
		  interesting-squares)))
    ; pair of state, square-info. for now
    (set-car! old (copy-state state '() states))
    (set-cdr! old square-info)
    (if new
	(set! square-info (cdr new))
	(square-info-init))
    (if new
	(copy-state (car new) state states)
	(create-level state))))


(define (send-event state event . arg)
  (fold (lambda (task state)
	  (apply task state event arg))
	state
	(get-state state 'tasks)))