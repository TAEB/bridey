(define buf #f)
(define colors #f)
(define cur #f)
(define cur-save #f)
(define color #f)

(define last (- (* 80 24) 1))
(define bold-bit 3)
(define inverse-bit 4)

(define check? #f)

(define partial #f)

(define (term-init)
  (set! buf (make-byte-vector (* 80 24) 0))
  (set! colors (make-byte-vector (* 80 24) 0))
  (set! cur 0)
  (set! color 0))

(define (term-got-partial?)
  partial)

(define (num->color n)
  (case n
    ((0)  'none)
    ((1)  'red)
    ((2)  'green)
    ((3)  'brown)
    ((4)  'blue)
    ((5)  'magenta)
    ((6)  'cyan)
    ((7)  'gray)
    ((8)  'black) ; light black, shows as dark gray. use_darkgray patch.
    ((9)  'orange)
    ((10) 'bright-green)
    ((11) 'yellow)
    ((12) 'bright-blue)
    ((13) 'bright-magenta)
    ((14) 'bright-cyan)
    ((15) 'white)
    (else 'octarine)))

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

(define (square-glyph-dir dir)
  (square-glyph (map + (get-coord) dir)))

(define (square-glyph coord)
  (let ((i (coord->i coord)))
    (list (num->color (unset-bit (byte-vector-ref colors i) inverse-bit))
	  (buf-char i))))

(define (iterate-screen f seed . opt)
  ; defaults to '(1 2) and '(80 22)
  (let ((start (if (null? opt) 80 (coord->i (car opt))))
	(end (if (null? opt) 1759 (coord->i (cadr opt)))))
    (let loop ((i start)
	       (s seed))
      (if (> i end)
	  s
	  (loop (+ i 1)
		(f s
		   (i->coord i)
		   (list (num->color (unset-bit (byte-vector-ref colors i)
						inverse-bit))
			 (buf-char i))))))))

(define (term-find-symbol find-glyph . opt)
  (let ((ls (apply iterate-screen
		   (lambda (ls coord glyph)
		     (if (equal? glyph find-glyph)
			 (cons coord ls)
			 ls))
		   '()
		   opt)))
    (and (not (null? ls))
	 ls)))

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

(define (make-piece str start len)
  (vector str start len))

(define (piece-length piece)
  (vector-ref piece 2))

(define (piece-ref piece i)
  (string-ref (vector-ref piece 0)
	      (+ (vector-ref piece 1) i)))

(define (piece-append a b)
  (let* ((a-len (piece-length a))
	 (b-len (piece-length b))
	 (len (+ a-len b-len))
	 (str (make-string len)))
    (do ((i 0 (+ i 1)))
	((= i a-len))
      (string-set! str i (piece-ref a i)))
    (do ((i 0 (+ i 1))
	 (j a-len (+ j 1)))
	((= i b-len))
      (string-set! str j (piece-ref b i)))
    (make-piece str 0 len)))

(define (piece->string piece)
  (let ((start (vector-ref piece 1)))
    (substring (vector-ref piece 0)
	       start
	       (+ start (vector-ref piece 2)))))

(define (piece-substring piece s e)
  (let ((start (vector-ref piece 1)))
    (substring (vector-ref piece 0)
	       (+ start s)
	       (+ start e))))

(define (break str)
  (let ((len (string-length str)))
    (let loop ((i 0)
	       (state (if partial 'command 'text))
	       (start 0)
	       (last-char #f)
	       (acc '()))
      (if (>= i len)
	  (let ((p (make-piece str start (- i start))))
	    (if (eq? state 'command)
		(begin (set! partial
			     (if partial
				 (piece-append partial p)
				 p))
		       (reverse acc))
		(reverse (cons p acc))))
	  (let ((c (string-ref str i)))
	    (case state
	      ((command)
	       (if (or (char-alphabetic? c)
		       (and (member c '(#\7 #\8))
			    (or (and last-char (char=? last-char #\escape))
				(and partial
				     (= (piece-length partial) 1)
				     (char=? (piece-ref partial 0) #\escape)))))
		   (let ((p (make-piece str start (+ 1 (- i start)))))
		     (if partial
			 (begin (set! p (piece-append partial p))
				(set! partial #f)))
		     (loop (+ i 1)
			   'text
			   (+ i 1)
			   c
			   (cons p acc)))
		   (loop (+ i 1)
			 'command
			 start
			 c
			 acc)))
	      ((text)
	       (if (char=? c #\escape)
		   (loop (+ i 1)
			 'command
			 i
			 c
			 (cons (make-piece str start (- i start))
			       acc))
		   (loop (+ i 1)
			 'text
			 start
			 c
			 acc)))))))))

(define unknown '())

(define (log-unknown str)
  (set! unknown (cons str unknown)))

(define (string-split-by-char str del)
  (let ((len (string-length str)))
    (let loop ((i 0)
	       (acc '())
	       (start 0))
      (cond ((>= i len)
	     (reverse (cons (substring str start i) acc)))
	    ((char=? (string-ref str i) del)
	     (loop (+ i 1)
		   (cons (substring str start i) acc)
		   (+ i 1)))
	    (else
	     (loop (+ i 1)
		   acc
		   start))))))

(define (escape-sequence? piece)
  (and (>= (piece-length piece) 2)
       (char=? (piece-ref piece 0) #\escape)))

(define (do-escape-sequence piece)
  (call/cc
   (lambda (exit)
     (define (fail)
       (log-unknown (piece->string piece))
       (exit))
     (cond
      ((= (piece-length piece) 2)
       (case (piece-ref piece 1)
	 ((#\7) (remember #f #f #f))
	 ((#\8) (restore #f #f #f))
	 (else (fail))))
      ((not (char=? (piece-ref piece 1) #\[)) (fail))
      (else
       (let ((param (piece-substring piece 2 (- (piece-length piece) 1))))
	 (do-command
	  (piece-ref piece (- (piece-length piece) 1))
	  (map (lambda (str)
		 (and (not (string=? str ""))
		      (or (string->number str)
			  (fail))))
	       (string-split-by-char param #\;)))))))))

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

(define (do-command command param)
  (define (char->num ch) (- (char->ascii ch) 48))
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
	  (list #\s remember) (list #\u restore))))
    (if (char=? command #\m)
	(if (equal? param '(#f))
	    (set-graphics-mode 0)
	    (for-each set-graphics-mode param))
	(let ((r (assv command handlers)))
	  (if (not r)
	      (log-unknown (list command param))
	      ((cadr r)
	       (and (>= (length param) 1) (car param))
	       (and (>= (length param) 2) (cadr param))
	       command))))))

(define (term-process str)
  (for-each
   (lambda (piece)
     (if (escape-sequence? piece)
	 (do-escape-sequence piece)
	 (do ((i 0 (+ i 1)))
	     ((= i (piece-length piece)))
	   (case (char->ascii (piece-ref piece i))
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
	     (else (insert-char (piece-ref piece i)))))))
   (break str)))
