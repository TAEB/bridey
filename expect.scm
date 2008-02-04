(define avatar #\@)

; don't use this unless you're sure the screen is going to change
(define (expect-generic res tries)
  (and (not (string=? res ""))
       (or (at-more?)
	   (at-question?)
	   (at-menu?)
	   (and avatar ; not invisible
		(char=? (square-char (get-coord)) avatar)
		(botl-update)
		; often a delay before topl gets printed, so extra read if topl
		; is empty
		(or (not (char=? (square-char '(1 1)) #\space))
		    (> tries 2)))
	   (> tries 4))))

(define (expect-menu res tries)
  (and (not (string=? res ""))
       (at-menu?)))

(define (expect-more res tries)
  (and (not (string=? res ""))
       (at-more?)))

(define (expect-no-change res tries)
  (> tries 4))

(define (expect-dunno res tries)
  (> tries 5))