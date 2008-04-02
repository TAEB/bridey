(define (get-inventory)
  (define (getstr x y)
    (string-trim-right (get-row-plaintext y x 80) #\space))
  (send-expect "i" expect-menu)
  (let* ((x (car (get-coord)))
 	 (y (cadr (get-coord)))
 	 (start-col (if (match-before-cur? "(end) ") (- x 6) 1)))
    (let loop ((ls '())
	       (row 1))
      (cond ((= row y)
 	     (if (or (match-before-cur? "(end) ")
 		     (at-last-page?))
		 (begin (send-expect " " expect-generic) ls)
		 (begin (send-expect " " expect-menu) (loop ls 1))))
	    ((square-inverse? (list start-col row))
	     (loop ls (+ row 1)))
	    (else
	     (let ((s (getstr start-col row)))
	       (if (inventory-item? s)
		   (loop (cons (split-inventory-item s) ls)
			 (+ row 1))
		   (begin (display "get-inventory: weird line: ")
			  (write s)
			  (display "!\n")))))))))

(define (get-discoveries)
  (let ((start-col
	 (send-expect "\\"
		      (lambda (res tries)
			(and (at-more?)
			     (+ 1 (string-contains (get-row-plaintext 1)
						   "Discoveries")))))))

    (let loop ((ls '()))
      (if (not (at-more?))
	  ls
	  (let ((lines (map (lambda (y)
			      (string-trim-right
			       (get-row-plaintext y start-col)
			       #\space))
			    (range 1 (- (cadr (get-coord)) 1)))))
	    (send-expect " " expect-generic)
	    (loop
	     (append
	      (filter-map
	       (lambda (line)
		 (and (or (string-prefix? "  " line)
			  (string-prefix? "* " line))
		      (let ((paren (string-index line #\()))
			(list (substring line 2 (- paren 1))
			      (substring line
					 (+ paren 1)
					 (- (string-length line) 1))))))
	       lines)
	      ls)))))))

(define (get-objects-here state)
  (define (getline y)
    (string-trim-right
     (get-row-plaintext y (if (or (= (cadr (get-coord)) 24)
				  (not (botl-visible?)))
			      1
			      (- (car (get-coord)) 8)))))
  (define (things-here-string? s)
    (or (string=? s "Things that are here:")
	(string=? s "Things that you feel here:")))
  (let* ((topline (getline 1))
	 (end-row (- (cadr (get-coord)) 1))
	 (start-row
	  (cond ((things-here-string? topline) 2)
		((things-here-string? (getline 3)) 4)
		(else 1)))
	 (first-page? (not (= start-row 1)))
	 (objects (map getline (range start-row end-row))))
    (send-expect " " expect-generic)
    (let ((ls (if first-page?
		  objects
		  (append objects (get-state state 'objects-here)))))
      (if (<= start-row 2)
	  (set-state state 'objects-here ls)
	  (set-state state
		     'objects-here ls
		     'messages (cons topline
				     (get-state state 'messages)))))))
	      
(define (read-topl state)
  (cond ((at-question?) ((get-state state 'question-handler) state))
	((and (at-more?) (or (> (cadr (get-coord)) 4)
			     (not (botl-visible?))))
	 (read-topl (get-objects-here state)))
	(else
	 (let ((state (set-state
		       state
		       'messages (append (reverse (read-messages))
					 (get-state state 'messages)))))
	   (if (at-more?)
	       (begin (send-expect " " expect-generic)
		      (read-topl state))
	       (begin (if (not (equal? (get-coord)
				       (get-state state 'expected-coord)))
			  (read-expect (lambda (res tries) (> tries 2))))
		      state))))))

(define (do-look state)
  (send-expect ":" expect-dunno)
  (read-topl state))

(define (redraw-screen)
  (send-expect (char->control-string #\R) expect-dunno))

(define (far-look coord)
  (let ((starting (get-coord)))
    (send-expect
     (string-append ";" (select-coord starting coord) ".")
     (lambda (res tries)
       (and-let* ((msgs (read-messages))
		  ((= (length msgs) 1))
		  (str (car msgs))
		  ((char=? (string-ref str 0) (square-char coord)))
		  ((string-every #\space str 1 8))
		  (start (string-index str #\())
		  (end (string-index str #\)))
		  ((> end start)))
	 (if (at-more?)
	     (send-expect " " expect-generic))
         (substring str (+ start 1) end))))))
