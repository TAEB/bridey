(define (find-walls state x/y in-column?)
  (define (p coord)
    (and (= 1 (count unseen?
		     (filter valid-coord?
			     (let ((d (if in-column? '(1 0) '(0 1))))
			       (list (map + coord d)
				     (map - coord d))))))
	 (and (eq? 'black (square-color coord))
	      (member (square-char coord) '(#\| #\-)))))
  (let ((ls (map (lambda (x/y2)
		   (let ((coord (if in-column?
				    (list x/y x/y2)
				    (list x/y2 x/y))))
		     (and (p coord) coord)))
		 (if in-column?
		     (reverse-range 20 4)
		     (reverse-range 78 3)))))
    (let loop ((ls ls)
	       (cur '())
	       (acc '()))
      (if (null? ls)
	  (if (>= (length cur) 3)
	      (cons cur acc)
	      acc)
	  (if (car ls)
	      (loop (cdr ls)
		    (cons (car ls) cur)
		    acc)
	      (loop (cdr ls)
		    '()
		    (if (>= (length cur) 3)
			(cons cur acc)
			acc)))))))

(define (grow-wall wall vert?)
  (define (grow start dir)
    (let loop ((c (map + start dir))
	       (acc '()))
      (if (or (seen? c)
	      (not (valid-coord? c)))
	  acc
	  (loop (map + c dir)
		(cons c acc)))))
  (let ((dir (if vert? '(0 1) '(1 0))))
    (append (grow (car wall) (map - dir))
	    wall
	    (reverse (grow (last wall) dir)))))

(define (unseen? c) (not (seen? c)))

(define (wall-dir wall vert?)
  (let* ((d (if vert? '(1 0) '(0 1)))
	 (side (map (lambda (c) (map + c d)) wall)))
    (cond ((every unseen? (map (lambda (c) (map + c d)) wall))
	   d)
	  ((every unseen? (map (lambda (c) (map - c d)) wall))
	   (map - d))
	  (else #f))))

(define (wall-extend wall vert?)
  (let ((dir (wall-dir wall vert?)))
    (if (not dir)
	0
	(let loop ((i 2))
	  (let ((disp (map (specialize * i) dir)))
	    (if (or (not (valid-coord? (map + (car wall) disp)))
		    (not (every unseen? (map (lambda (c) (map + c disp))
					     wall))))
		(- i 1)
		(loop (+ i 1))))))))

; For squares, the score is the same as the area of the unexplored area
; extending out from the wall. We prefer rectangles that are closer
; to squares. To adjust the score, we multiply the area by the fourth root
; of the height/width ratio. So while 3x3 has a score of 9, 1x9 has a score
; of only (* 1 9 (expt (/ 1 9) 1/4)) = 5.20.
(define (wall-score wall vert?)
  (let ((extend (wall-extend wall vert?)))
    (if (zero? extend)
	0
	(let* ((len (length wall))
	       (ratio (if (< len extend)
			  (/ len extend)
			  (/ extend len)))
	       (penalty (expt ratio 1/4)))
	  (* len extend penalty)))))

(define (walls-to-search state)
  (define (ranked-walls vert?)
    (let ((walls
	   (apply append
		  (map (lambda (x/y)
			 (find-walls state x/y vert?))
		       (if vert?
			   (range 1 80)
			   (range 2 22))))))
      (map (lambda (wall)
	     (list wall (wall-score (grow-wall wall vert?) vert?)))
	   walls)))
  (map car
       (list-sort (lambda (a b)
		    (> (cadr a) (cadr b)))
		  (append (ranked-walls #t)
			  (ranked-walls #f)))))
