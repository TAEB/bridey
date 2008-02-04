; need to import regexps wholesale... mind the massive namespace pollution.

(define page-num (sequence (text "(")
			   (submatch 'cur (repeat 1 #f numeric))
			   (text " of ")
			   (submatch 'total (repeat 1 #f numeric))
			   (text ")")))

(define (match-before-cur? str)
  (let ((len (string-length str)))
    (and (> (car (get-coord)) len)
	 (term-match-string? str (map - (get-coord) (list len 0))))))

(define (at-question?)
  (and (char=? (square-char (get-coord)) #\space) ; nethack clears the line, right?
       (or (term-match-string? "(For instructions type a ?)" '(1 1))
	   (and (term-match-string? "Call " '(1 1))
		(match-before-cur? ": "))
	   (match-before-cur? " [yn] (n) ")
	   (match-before-cur? " [ynaq] (y) ")
	   ; more prompt types?
	   )))

; parse (n of n)
; makes me wish I still had perl :(
(define (menu-page/page)
  (let* ((c (get-coord))
	 (x (car c)) (y (cadr c)))
    (call/cc
     (lambda (ret)
       (letrec ((next-char
		 (lambda ()
		   (if (= x 1)
		       (ret #f)
		       (begin (set! x (- x 1))
			      (square-char (list x y))))))
		(read-num
		 (lambda ()
		   (let ((c (next-char)))
		     (if (not (char-numeric? c))
			 (begin (set! x (+ x 1)) 0)
			 (+ (char->number c) (* 10 (read-num)))))))
		(match-string
		 (lambda (str)
		   (string-every (lambda (c) (char=? c (next-char)))
				 (string-reverse str)))))
	 (and-let* (((match-string ") "))
		    (total (read-num))
		    ((> total 1))
		    ((match-string " of "))
		    (current (read-num))
		    ((> current 0))
		    ((match-string "(")))
	   (list current total)))))))
	    
(define (at-menu?)
  (or (match-before-cur? "(end) ")
      (menu-page/page)))

(define (at-last-page?) ; of a menu
  (let* ((s (get-row-plaintext (cadr (get-coord))))
	 (m (match page-num s)))
    (if (not m)
	(display "scraper: missing pager!\n"))
    (let* ((sm (match-submatches m))
	   (cur-m (cdr (assq 'cur sm)))
	   (total-m (cdr (assq 'total sm)))
	   (cur (substring s (match-start cur-m) (match-end cur-m)))
	   (total (substring s (match-start total-m) (match-end total-m))))
      (string=? cur total))))

(define (read-messages)
  (define (join ls)
    (string-join
     (map (lambda (s) (string-trim-right s #\space))
	  ls)))
  (split-messages
   (if (not (at-more?))
       (string-trim-right (get-row-plaintext 1) #\space)
       (let ((x (car (get-coord)))
	     (y (cadr (get-coord))))
	 (join (append (map get-row-plaintext
			    (iota (- y 1) 1 1))
		       (list (get-row-plaintext y 1 (- x 9)))))))))

(define (split-messages str)
  (define (punc? i)
    (memv (string-ref str i) '(#\. #\! #\?)))
  (define (c=? i c)
    (char=? (string-ref str i) c))
  (let ((len (string-length str)))
    (let loop ((ls '())
	       (start 0)
	       (i 0)
	       (spaces #f))
      (cond
       ((= i len)
	(reverse (cons (substring str start i) ls)))
       ((not spaces)
	(loop ls start (+ i 1) (and (punc? i) 0)))
       ((and (zero? spaces)
	     (c=? i #\")
	     (not (c=? (- i 1) #\")))
	(loop ls start (+ i 1) 0))
       ((and (= spaces 1) (c=? i #\space))
	(loop (cons (substring str start (- i 1)) ls) (+ i 1) (+ i 1) #f))
       (else
	(loop ls start (+ i 1) (and (c=? i #\space) (+ spaces 1))))))))

(define (at-more?) (match-before-cur? "--More--"))

(define (inventory-item? str)
  (and (> (string-length str) 5)
       (char-alphabetic? (string-ref str 0))
       (char=? (string-ref str 1) #\space)
       (char=? (string-ref str 2) #\-)
       (char=? (string-ref str 3) #\space)))

(define (split-inventory-item str)
  (list (string-ref str 0)
	(string-drop str 4)))

