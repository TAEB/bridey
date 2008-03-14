(define iac (integer->char 255))
(define sb (integer->char 250))
(define se (integer->char 240))
(define will (integer->char 251))
(define wont (integer->char 252))
(define please-do (integer->char 253)) ; I need my loops
(define dont (integer->char 254))
(define ping! (integer->char 99)) ; Sartak's world-famous telnet ping
(define is (integer->char 0))

(define ping-string (string iac please-do ping!))

(define terminal-type (integer->char 24))
(define naws (integer->char 31))

(define buf #f)
(define pos #f)
(define in #f)
(define out #f)
(define size 1024)

(define return #f)

(define (telnet-init)
  (set! buf (make-byte-vector size 0))
  (set! pos 0)
  (call-with-values
      (lambda () (socket-client "nethack.alt.org" 23))
    (lambda (i o)
      (set! in i)
      (set! out o)))
  (telnet-read-expect
   (lambda (res tries)
     (match-before-cur? "=> ")))
  (telnet-send-expect
   (string-append "lBrideyTest\n"
		  (call-with-input-file "passwd" read)
		  "\np")
   (lambda (res tries)
     (> tries 3))))


(define (telnet-end)
  (close-input-port in)
  (close-output-port out))

(define (getc)
  (let ((c (read-char in)))
    (if (eof-object? c)
	(return c)
	c)))

(define (puts s) (display s out))

(define (do-iac)
  (let ((c (getc)))
    (cond ((or (char=? c will)
	       (char=? c wont)
	       (char=? c dont))
	   (let ((opt (getc))
		 (ans (if (char=? c dont) wont dont)))
	     (puts (string iac ans opt))
	     (if (char=? opt ping!)
		 (wrap-up))))
	  ((char=? c please-do)
	   (let ((opt (getc)))
	     (cond ((char=? opt terminal-type)
		    (puts (string-append
			   (string iac will terminal-type
				   iac sb terminal-type is)
			   "xterm-color"
			   (string iac se))))
		   ((char=? opt naws)
		    (puts (string iac will naws
				  iac sb naws is
				  (integer->char 80)
				  (integer->char 0)
				  (integer->char 24)
				  (integer->char 0)
				  iac se)))
		   (else (puts (string iac wont opt))))))
	  ((char=? c sb)
	   (let loop ()
	     (if (not (char=? (getc) se))
		 (loop))))
	  ((char=? c iac)
	   (add-char c))
	  (else
	   (display "telnet: unknown code after IAC: ")
	   (write (char->integer c))
	   (newline)))))

(define (telnet-read)
  (call/cc
   (lambda (ret)
     (set! return ret)
     (puts ping-string)
     (let loop ((c (getc)))
       (if (char=? c iac)
	   (do-iac)
	   (add-char c))
       (loop (getc))))))

; escape IAC? meh
(define (telnet-send . ls)
  (for-each (lambda (x)
	      ((if (char? x) write-char display) x out))
	    ls)
  (term-process (telnet-read)))

(define (telnet-read-expect expect . opt)
  (let ((initial (if (null? opt) "" (car opt))))
    (let loop ((tries 1) (res initial))
      (or (expect res tries)
	  (loop (+ tries 1) (string-append res (telnet-read)))))))

(define (telnet-send-expect str expect)
  (puts str)
  (if expect
      (telnet-read-expect expect (telnet-read))))

(define (add-char c)
  (byte-vector-set! buf pos (char->integer c))
  (set! pos (+ pos 1))
  (if (= pos size)
      (let ((new (make-byte-vector (* size 2) 0)))
	(do ((i 0 (+ i 1)))
	    ((= i pos))
	  (byte-vector-set! new i (byte-vector-ref buf i)))
	(set! size (* size 2))
	(set! buf new))))

(define (wrap-up)
  (let ((str (make-string pos)))
    (do ((i 0 (+ i 1)))
	((= i pos))
      (string-set! str i (integer->char (byte-vector-ref buf i))))
    (set! pos 0)
    (term-process str)
    (return str)))
