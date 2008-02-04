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