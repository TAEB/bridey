(define buf #f)
(define colors #f)
(define cur #f)
(define cur-save #f)
(define color #f)

(define last (- (* 80 24) 1))
(define bold-bit 3)
(define inverse-bit 4)

(define check? #f)

(define (term-init)
  (set! buf (make-byte-vector (* 80 24) 0))
  (set! colors (make-byte-vector (* 80 24) 0))
  (set! cur 0)
  (set! color 0))

(define (num->color n)
  (case n
    ((0)  'black)
    ((1)  'red)
    ((2)  'green)
    ((3)  'brown)
    ((4)  'blue)
    ((5)  'magenta)
    ((6)  'cyan)
    ((7)  'gray)
    ((9)  'orange)
    ((10) 'bright-green)
    ((11) 'yellow)
    ((12) 'bright-blue)
    ((13) 'bright-magenta)
    ((14) 'bright-cyan)
    ((15) 'white)
    (else 'ultraviolet)))

(define (buf-char i)
  (let ((n (byte-vector-ref buf i)))
    (if (< n 32)
	#\space
	(ascii->char n))))

(define (square-char-dir dir)
  (square-char (map + (get-coord) dir)))

(define (square-char coord)
  (buf-char (coord->i coord)))

(define (square-color-dir dir)
  (square-color (map + (get-coord) dir)))

(define (square-color coord)
  (num->color (unset-bit (map-bv-ref colors coord) inverse-bit)))

(define (square-inverse-dir? dir)
  (square-inverse? (map + (get-coord) dir)))

(define (square-inverse? coord)
  (bit-set? (map-bv-ref colors coord) inverse-bit))

(define (iterate-screen f seed . opt)
  (let ((start (if (null? opt) 0 (coord->i (car opt))))
	(end (if (< (length opt) 2) last (coord->i (cadr opt)))))
    (let loop ((i start)
	       (s seed))
      (if (> i end)
	  s
	  (loop (+ i 1)
		(f s
		   (i->coord i)
		   (buf-char i)
		   (num->color (unset-bit (byte-vector-ref colors i)
					  inverse-bit))))))))

(define (term-find-symbol find-char find-color . opt)
  (apply iterate-screen
	 (lambda (ls coord char color)
	   (if (and (char=? find-char char)
		    (or (not find-color)
			(eq? find-color color)))
	       (cons coord ls)
	       ls))
	 '()
	 opt))

(define (get-row-plaintext y . opt)
  (let* ((start (if (null? opt) 0 (- (car opt) 1)))
	 (end (if (< (length opt) 2) 79 (- (cadr opt) 1)))
	 (len (+ (- end start) 1))
	 (str (make-string len))
	 (pos (+ (* (- y 1) 80) start)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (string-set! str i (buf-char (+ pos i))))
    str))

(define (term-match-string? str coord)
  (let ((end (min (- (string-length str) 1)
		  (- 81 (cadr coord))))
	(pos (coord->i coord)))
    (and (>= pos 0)
	 (let loop ((i 0))
	   (or (> i end)
	       (and (char=? (string-ref str i)
			    (buf-char (+ pos i)))
		    (loop (+ i 1))))))))

(define (get-coord)
  (i->coord cur))

(define (inc!)
  (if (< cur last)
      (set! cur (+ cur 1))))

(define (insert-char c)
  (if (or (< (char->ascii c) 32)
	  (= (char->ascii c) 127))
      (begin
	(display "term: printing control character: ")
	(write (char->ascii c))
	(newline)))
  (byte-vector-set! buf cur (char->ascii c))
  (byte-vector-set! colors cur color)
  (inc!))

(define (limit-coord coord)
  (let ((x (car coord))
	(y (cadr coord)))
    (list (if (> x 80) 80 (if (< x 1) 1 x))
	  (if (> y 24) 24 (if (< y 1) 1 y)))))

(define (term-draw)
  (define (line)
    (display (apply string (make-list 82 #\-)))
    (newline))
  (line)
  (do ((y 1 (+ y 1)))
      ((> y 24))
    (display "|")
    (do ((x 1 (+ x 1)))
	((> x 80))
      (display (square-char (list x y))))
    (display "|")
    (newline))
  (line))

(define (term-process str)
  (define (char->num ch) (- (char->ascii ch) 48))
  (define (goto c) (set! cur (coord->i (limit-coord c))))
  (define (move n m cntl)
    (let ((n (or n 1))
	  (coord (i->coord cur))
	  (dir (cond
		((char=? cntl #\A) '(0 -1))
		((char=? cntl #\B) '(0 1))
		((char=? cntl #\C) '(1 0))
		((char=? cntl #\D) '(-1 0)))))
      (goto (map + coord (map (specialize * n) dir)))))
  (define (moveln n m cntl)
    (let ((n (or n 1))
	  (x (car (i->coord cur)))
	  (y (cadr (i->coord cur)))
	  (op (if (char=? cntl #\E) + -)))
      (goto (list x (op y n)))))
  (define (moveto n m cntl)
    (let ((y (or n 1))
	  (x (or m 1)))
      (goto (list x y))))
  (define (remember n m cntl)
    (set! cur-save cur))
  (define (restore n m cntl)
    (if cur-save
	(set! cur cur-save)))
  (define (set-graphics-mode n)
    (cond ((zero? n) (set! color 0))
	  ((= n 1) (set! color (set-bit color bold-bit)))
	  ((= n 7) (set! color (set-bit color inverse-bit)))
	  ((and (>= n 30) (<= n 37))
	   ; first clear the color part -- the three lower bits
	   (set! color (bitwise-and (bitwise-not 7) color))
	   (set! color (bitwise-ior color (- n 30))))
	  (else (display "term: unknown graphics mode: ")
		(display n)
		(newline))))
  (let ((handlers
	 (list
	  (list #\A move) (list #\B move) (list #\C move) (list #\D move)
	  (list #\E moveln) (list #\F moveln)
	  (list #\G (lambda (n m cntl)
		      (goto (list n (cadr (i->coord cur))))))
	  (list #\H moveto) (list #\f moveto)
	  (list #\J (lambda (n m cntl)
		      (cond ((or (not n) (zero? n))
			     (do ((i cur (+ i 1)))
				 ((> i last))
			       (byte-vector-set! buf i 0)))
			    ((= n 1)
			     (do ((i 0 (+ 1)))
				 ((> i cur))
				 (byte-vector-set! buf i 0)))
			    ((= n 2)
			     (set! buf (make-byte-vector (* 80 24) 0))))))
	  (list #\K (lambda (n m cntl)
		      (let ((x (modulo cur 80)))
			(cond ((or (not n) (zero? n))
			       (do ((i 0 (+ i 1)))
				   ((> i (- 79 x)))
				 (byte-vector-set! buf (+ cur i) 0)))
			      ((= n 1)
			       (do ((i 0 (+ i 1)))
				   ((> i x))
				 (byte-vector-set! buf (- cur i) 0)))
			      ((= n 2)
			       (let ((y (quotient cur 80)))
				 (do ((i 0 (+ i 1)))
				     ((> i 79))
				   (byte-vector-set! buf (+ y i) 0))))))))
	  (list #\s remember) (list #\7 remember)
	  (list #\u restore) (list #\8 restore)))
	(len (string-length str))
	(i 0)
	(c #f))
    (call/cc
     (lambda (exit)
       (console-process-output str)
       (let ((next
	      (lambda ()
		(if (>= i len) (exit))
		(begin (set! c (string-ref str i))
		       (set! i (+ i 1))
		       c))))
	 (let loop ()
	   (if (not (char=? (next) #\escape))
	       (case (char->ascii c)
		 ; NL
		 ((10) (if (< cur (* 80 23))
			   (set! cur (+ cur 80 (- (modulo cur 80))))))
		 ; CR
		 ((13) (set! cur (- cur (modulo cur 80))))
		 ; BS
		 ((8) (if (not (zero? (modulo cur 80)))
			  (set! cur (- cur 1))))
		 ; NUL
		 ((0) 'nothing)
		 (else (insert-char c)))
	       (if (not (char=? (next) #\[))
		   (insert-char c)
		   (let loop ((acc #f)
			      (ls '()))
		     (cond ((char-numeric? (next))
			    (loop (+ (char->num c) (* (or acc 0) 10)) ls))
			   ((char=? c #\;)
			    (loop #f (cons acc ls)))
			   (else
			    (set! ls (reverse (if acc (cons acc ls) ls)))
			    (cond ((char=? c #\m)
				   (if (null? ls)
				       (set-graphics-mode 0)
				       (for-each set-graphics-mode ls)))
				  ((char=? c #\?) (loop #f '()))
				  (else
				   (if (> (length ls) 2)
				       (display "Term: too many parameters\n"))
				   (let ((r (assv c handlers)))
				     (if (not r)
					 (begin
					   (display "Term: unknown escape code: ")
					   (display c)
					   (display ", with params ")
					   (write ls)
					   (newline))
					 ((cadr r)
				       (and (>= (length ls) 1) (car ls))
				       (and (>= (length ls) 2) (cadr ls))
				       c))))))))))
	   (loop)))))))
    