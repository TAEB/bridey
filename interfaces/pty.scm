(define in #f)
(define out #f)

(define (pty-init)
  (let ((port (call-with-input-file "port" read)))
    (call-with-values
      (lambda () (socket-client "localhost" port))
      (lambda (i o)
	(set! in i)
	(set! out o)))))

(define (pty-end)
  (close-input-port in)
  (close-output-port out))

(define (get)
  (let ((len (read in)))
    (if (eof-object? len)
	len
	(let ((str (readn (car len))))
	  (term-process str)
	  str))))

(define (read-more)
  (write-char (integer->char 0) out)
  (get))

(define (pty-read-expect expect . opt)
  (let ((initial (if (null? opt) "" (car opt))))
    (let loop ((tries 1) (res initial))
      (if (> tries 20)
	  (list 'problem-with-expect expect)
	  (or (expect res tries)
	      (begin (write-char (integer->char 0) out)
		     (loop (+ tries 1) (string-append res (get)))))))))

(define (pty-send-expect str expect)
  (if expect
      (let ((res (string-concatenate
		  (map-in-order
		   (lambda (c) (write-char c out) (get))
		   (string->list str)))))
	(pty-read-expect expect res))
      (string-for-each
       (lambda (c) (write-char c out) (get))
       str)))

(define (readn n)
  (call/cc
   (lambda (exit)
     (let ((str (make-string n)))
       (do ((i 0 (+ i 1)))
	   ((= i n) str)
	 (let ((c (read-char in)))
	   (if (eof-object? c)
	       (begin (display "ask: got eof in readn") (exit c))
	       (string-set! str i c))))))))
