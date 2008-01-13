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
		 (monster-string? (cadr (far-look coord))))))))

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
	     (not (door? state coord))
	     (or (not (eq? (square-color coord) 'brown))
		 (any (lambda (room) (within-extents? room coord))
		      (get-state state 'rooms))))
	(and rogue?
	     (or (char=? ch #\,)
		 (char=? ch #\])
		 (and (char=? ch #\:)
		      (not (monster-string? (cadr (far-look coord)))))))
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
  (or (trap? state coord)
      (fountain? state coord)
      (throne? state coord)
      ;(altar? state coord)
      (char=? (square-char coord) #\{)
      (char=? (square-char coord) #\\)))

(define (trap-type state coord)
  (and (trap? state coord)
       (let ((e (assoc coord (get-state state 'traps))))
	 (if (not e)
	     (display "trap-type: missing trap\n")
	     (cadr e)))))

; incomplete..?
(define (square-clear? state coord)
  (case (square-char coord)
    ((#\. #\{ #\^ #\{#\{#\\) #t)
    ((#\#) (not (memq (square-color coord)
		      '(cyan green whatever-drawbridge-is))))
    ((#\_) (not (eq? (square-color coord) 'cyan)))
    ((#\") (not (eq? (square-color coord) 'amulet-color)))
    (else #f)))

(define (within-extents? ext . opt) ; can't be on the line
  (let* ((nw (car ext)) (ne (cadr ext))
	 (coord (if (null? opt) (get-coord) (car opt))))
    (and (eq? (dir->vi (map - coord nw)) 'n)
	 (eq? (dir->vi (map - coord ne)) 'y))))

(define (dir->vi dir)
  (let ((dx (car dir)) (dy (cadr dir)))
    (cond ((and (= dx 0) (= dy 0)) #f)
	  ((= dx 0) (if (> dy 0) 'j 'k))
	  ((= dy 0) (if (> dx 0) 'l 'h))
	  ((> dx 0) (if (> dy 0) 'n 'u))
	  ((< dx 0) (if (> dy 0) 'b 'y)))))

(define (vi->dir vi)
  (let* ((ls '((j (0 1)) (k (0 -1))
	      (l (1 0)) (h (-1 0))
	      (n (1 1)) (u (1 -1))
	      (b (-1 1)) (y (-1 -1))))
	 (res (assoc vi ls)))
    (and res (cadr res))))

;; (define (nsteps-to xy . opt)
;;   (let* ((pos (if (null? opt) (get-coord) (car opt)))
;; 	 (dir (step-toward xy pos)))
;;     (if (equal? dir '(0 0))
;; 	0
;; 	(+ 1 (nsteps-to xy (map + pos dir))))))

(define (eight-dirs)
  (map vi->dir '(h y k u l n j b)))

(define (neighbor-squares . opt)
  (let ((coord (if (null? opt) (get-coord) (car opt))))
    (filter
     (lambda (c)
       (and (>= (car c) 1) (<= (car c) 80)
	    (>= (cadr c) 1) (<= (cadr c) 24)))
     (map (specialize map + coord) (eight-dirs)))))

(define (diagonal? dir)
  (not (= (abs (apply + dir)) 1)))

(define (weird-position? state)
  (not (equal? (get-coord) (get-state state 'expected-coord))))

(define (open-door? state coord)
  (and (door? state coord)
       (not (char=? (square-char coord) #\+))))

(define (boulder? coord)
  (char=? (square-char coord) #\0))

; TODO
(define (bad-trap? state coord) #f)

;(define (touching? a b)
;  (= 1 (nsteps-to a b)))

(define (dead-end? state . opt)
  (let ((coord (if (null? opt) (get-coord) (car opt))))
    (= 3 (count
	  (lambda (c)
	    (and (not (diagonal? (map - c coord)))
		 (or (char=? (square-char c) #\space)
		     (wall? c))))
	  (neighbor-squares coord)))))

(define (passable? state to from)
  (let ((dir (map - to from)))
    (not (or (wall? to)
	     (char=? (square-char to) #\space)
	     (and (boulder? to)
		  (member (list to dir) (get-state state 'stuck-boulders)))
	     (and (or (door? state to)
		      (door? state from))
		  (diagonal? dir))
	     (and (embedded? state to)
		  (not (door? state to)))))))

(define (internal-visited? bit state coord)
  (let ((vec (get-state state 'visited))
	(x (- (car coord) 1))
	(y (- (cadr coord) 1)))
    (bit-set? (byte-vector-ref vec (+ (* y 80) x)) bit)))

(define (internal-mark-visited bit state coord)
  (let* ((vec (get-state state 'visited))
	 (x (- (car coord) 1))
	 (y (- (cadr coord) 1))
	 (index (+ (* y 80) x)))
    (byte-vector-set! vec index (set-bit (byte-vector-ref vec index) bit))))

(define (internal-unmark-visited bit state coord)
  (let* ((vec (get-state state 'visited))
	 (x (- (car coord) 1))
	 (y (- (cadr coord) 1))
	 (index (+ (* y 80) x)))
    (byte-vector-set! vec index (unset-bit (byte-vector-ref vec index) bit))))

(define visited? (specialize internal-visited? 0))
(define mark-visited (specialize internal-mark-visited 0))

(define seen? (specialize internal-visited? 3))
(define mark-seen (specialize internal-mark-visited 3))

(define internal-door? (specialize internal-visited? 4))
(define mark-door (specialize internal-mark-visited 4))
(define unmark-door (specialize internal-unmark-visited 4))

(define trap? (specialize internal-visited? 5))
(define mark-trap (specialize internal-mark-visited 5))
(define unmark-trap (specialize internal-unmark-visited 5))

(define embedded? (specialize internal-visited? 6))
(define mark-embedded (specialize internal-mark-visited 6))
(define unmark-embedded (specialize internal-unmark-visited 6))

(define (door? state coord)
  (or (internal-door? state coord)
      (and (eq? (square-color coord) 'brown)
	   (let ((char (square-char coord)))
	     (or (char=? char #\|)
		 (char=? char #\-)))
	   (begin (mark-door state coord) #t))))

(define (wall? coord)
  (and (not (eq? (square-color coord) 'brown))
       (or (char=? (square-char coord) #\|)
	   (char=? (square-char coord) #\-))))

(define (searched-for state coord)
  (let ((i (+ (* (- (cadr coord) 1) 80)
	      (- (car coord) 1))))
    (byte-vector-ref (get-state state 'searched) i)))

