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
    (map (lambda (wall)
	   (list wall (wall-score (grow-wall wall vert?) vert?)))
	 (apply append
		(map (lambda (x/y)
		       (find-walls state x/y vert?))
		     (if vert?
			 (range 1 80)
			 (range 2 22))))))
  (map car
       (list-sort (lambda (a b)
		    (> (cadr a) (cadr b)))
		  (append (ranked-walls #t)
			  (ranked-walls #f)))))

(define (wall-get-inner state wall)
  (let* ((dir (map - (cadr wall) (car wall)))
	 (disp (map abs (reverse dir))))
    (let ((side (map (lambda (c) (map + c disp)) wall)))
      (if (every seen? side)
	  side
	  (let ((side (map (lambda (c) (map - c disp)) wall)))
	    (if (every seen? side)
		side
		'wall-get-inner-missed))))))

(define (search-walls state)
  (let* ((current (get-state state 'wall-searching))
	 (walls (get-state state 'walls-to-search)))
    (cond ((not walls)
	   (search-walls
	    (set-state state 'walls-to-search (walls-to-search state))))
	  ((not current)
	   (if (null? walls)
	       (pop-action-go (set-state state 'searched-walls #t))
	       (let* ((inner (wall-get-inner state (car walls)))
		      (a (find-path-hard state (get-coord) (car inner)))
		      (b (find-path-hard state (get-coord) (last inner))))
		 (search-walls (set-state
				state
				'walls-to-search (cdr walls)
				'wall-searching (if (< (length a) (length b))
						    inner
						    (reverse inner)))))))
	  ((and (< (searched-for state (get-coord)) 23)
		(member (get-coord) current))
	   (search state))
	  (else
	   (let ((rest (remove (specialize adjacent? (get-coord)) current))
		 (p (specialize find-path-to state (get-coord))))
	     (if (and (> (length rest) 1)
		      (p (cadr rest)))
		 (push-action-go (set-state state 'wall-searching rest)
				 (lambda (state) (go-to state (cadr rest))))
		 (let loop ((rest rest))
		   (cond ((null? rest)
			  (search-walls (set-state state 'wall-searching #f)))
			 ((p (car rest))
			  (push-action-go
			   (set-state state 'wall-searching rest)
			   (lambda (state) (go-to state (car rest)))))
			 (else (loop (cdr rest)))))))))))
		   
		  
	  