(define nao? #f)

(define send #f)
(define send-expect #f)
(define read-expect #f)

(define (char-selection)
  (string-for-each
   (lambda (c)
     (send-expect (string c) expect-generic))
   "nbhmc "))

(define (nethack-init)
  (if nao?
      (begin (set! read-expect telnet-read-expect)
	     (set! send-expect telnet-send-expect)
	     (telnet-init))
      (begin (set! read-expect pty-read-expect)
	     (set! send-expect pty-send-expect)
	     (pty-init)))
  (read-expect
   (lambda (res tries)
     (cond ((match-before-cur? "for you? [ynq] ")
	    (char-selection))
	   ((match-before-cur? "Restoring save file...--More--")
	    (send-expect " " expect-generic))
	   ((string-prefix? "There are some stale nethack processes"
			    (get-row-plaintext 3))
	    (system "sleep 10")
	    #f)
	   (else #f)))))

(define (nethack-redraw)
  (term-init)
  (send-expect (char->control-string #\R) expect-dunno))
