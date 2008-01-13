(define (min-p <? head . rest)
  (cond
   ((null? rest) head)
   ((<? head (car rest)) (apply min-p <? head (cdr rest)))
   (else (apply min-p <? rest))))

(define (select-coord from to)
  (define (lim-x n) (if (> n 80) 80 (if (< n 0) 0 n)))
  (define (lim-y n) (if (> n 24) 24 (if (< n 0) 0 n)))
  (define (distance a b)
    (max (abs (- (car a) (car b)))
	 (abs (- (cadr a) (cadr b)))))
  (define (path-to from to)
    (let ((dis (distance from to))
	  (dx (- (car to) (car from)))
	  (dy (- (cadr to) (cadr from))))
      (map list
	   (append (make-list (abs dx) (if (zero? dx) 0 (/ dx (abs dx))))
		   (make-list (- dis (abs dx)) 0))
	   (append (make-list (abs dy) (if (zero? dy) 0 (/ dy (abs dy))))
		   (make-list (- dis (abs dy)) 0)))))
  (define (dir->vi-char dir)
    (let ((dx (car dir)) (dy (cadr dir)))
      (cond ((and (= dx 0) (= dy 0)) #f)
	    ((= dx 0) (if (> dy 0) #\j #\k))
	    ((= dy 0) (if (> dx 0) #\l #\h))
	    ((> dx 0) (if (> dy 0) #\n #\u))
	    ((< dx 0) (if (> dy 0) #\b #\y)))))
  (let* ((old-x (car from))
	 (old-y (cadr from))
	 (new-x (car to))
	 (new-y (cadr to))
	 (dx (- new-x old-x))
	 (dy (- new-y old-y))

	 (left-x (floor (/ dx 8)))
	 (right-x (ceiling (/ dx 8)))
	 (upper-y (floor (/ dy 8)))
	 (lower-y (ceiling (/ dy 8)))

	 (x1 (lim-x (+ old-x (* 8 left-x))))
	 (x2 (lim-x (+ old-x (* 8 right-x))))
	 (y1 (lim-y (+ old-y (* 8 upper-y))))
	 (y2 (lim-y (+ old-y (* 8 lower-y))))

	 ; (coord keystrokes)
	 (nw (list (list x1 y1) (path-to '(0 0) (list left-x upper-y))))
	 (ne (list (list x2 y1) (path-to '(0 0) (list right-x upper-y))))
	 (sw (list (list x1 y2) (path-to '(0 0) (list left-x lower-y))))
	 (se (list (list x2 y2) (path-to '(0 0) (list right-x lower-y))))

	 ; (keystrokes keystrokes)
	 (paths (map (lambda (e)
		       (list (cadr e) (path-to (car e) to)))
		     (list nw ne sw se)))

	 (path (apply
		min-p (lambda (a b)
			(< (length (apply append a))
			   (length (apply append b))))
		paths))

	 (keystrokes
	  (append (map (compose char-upcase dir->vi-char) (car path))
		  (map dir->vi-char (cadr path)))))
    (list->string keystrokes)))



