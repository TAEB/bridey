; don't know to write this as a R5RS macro...
(define-syntax botl-commands
  (lambda (form rename compare)
    (define (make-def name)
      (list 'define (list name) (list 'botl-get (list 'quote name))))
    (cons 'begin (map make-def (cdr form)))))

(botl-commands str dex con int wis cha align score
	       dlvl gold curhp maxhp curpw maxpw ac xlvl xp turns)

;TODO: status, esp. satiated and stunned
(define data
  (map (lambda (x) (list x #f))
       '(str dex con int wis cha align score
	 dlvl gold curhp maxhp curpw maxpw ac xlvl xp turns)))

(define (botl-set! key value)
  (set-cdr! (assq key data) (list value)))

(define (botl-get key)
  (cadr (assq key data)))

(define bot1
  (sequence
    (text "St:")
    (submatch 'str (one-of (sequence (text "18/")
				     (submatch 'str-18
					       (one-of (text "**")
						       (repeat 2 numeric))))
			   (repeat 1 2 numeric)))
    (apply
     sequence
     (map (lambda (tag key)
	    (sequence (text (string-append " " tag ":"))
		      (submatch key (repeat 1 2 numeric))))
	  '("Dx" "Co" "In" "Wi" "Ch")
	  '(dex con int wis cha)))
    (text "  ")
    (submatch 'align (apply one-of (map text '("Lawful" "Neutral" "Chaotic"))))
    (repeat 0 1 (sequence (text " S:") (submatch 'score (repeat numeric))))
    (repeat whitespace)
    (string-end)))

(define bot2
  (let ((num (repeat 1 #f numeric)))
    (sequence
      (string-start)
      (submatch 'branch
		(one-of (text "Astral Plane")
			(text "End Game")
			(text "Fort Ludios")
			(sequence (text "Home ")
				  (submatch 'dlvl numeric))
			(sequence (text "Dlvl:")
				  (submatch 'dlvl (repeat 1 2 numeric)))))
      (repeat whitespace)
      (one-of (text "$") (text "*"))
      (text ":")
      (submatch 'gold num)
      (repeat whitespace)
      (text "HP:")
      (submatch 'curhp num)
      (text "(")
      (submatch 'maxhp num)
      (text ") Pw:")
      (submatch 'curpw num)
      (text "(")
      (submatch 'maxpw num)
      (text ") AC:")
      (submatch 'ac (one-of (sequence (text "-") num)
			    num))
      (repeat whitespace)
      (one-of (sequence (one-of (text "HD") (text "Exp"))
			(text ":")
			(submatch 'xlvl (repeat 1 2 numeric)))
	      (sequence (text "Xp:")
			(submatch 'xlvl (repeat 1 2 numeric))
			(text "/")
			(submatch 'xp num)))
      (text " T:")
      (submatch 'turns num)
      (repeat whitespace))))

(define (botl-visible?)
  (call/cc
   (lambda (exit)
     (iterate-screen
      (lambda (seed coord glyph)
	(if (not (char=? (cadr glyph) #\space))
	    (exit #t)
	    seed))
      #f
      '(1 24)
      '(15 24)))))

(define (botl-update)
  (call/cc
   (lambda (ret)
     (define (foreach-botl regex str)
       (for-each
	(lambda (submatch)
	  (let ((key (car submatch))
		(value (substring str
				  (match-start (cdr submatch))
				  (match-end (cdr submatch)))))
	    (case key
	      ((aling) (cond ((string=? value "Lawful") 'lawful)
			     ((string=? value "Neutral") 'neutral)
			     ((string=? value "Chaotic") 'chaotic)
			     (else 'wtf-man)))
	      ((align) (botl-set! 'align value))
	      ((str) (if (not (string-prefix? "18/" value))
			 (botl-set! 'str (string->number value))))
	      ((str-18) (botl-set!
			 'str
			 (if (string=? value "**")
			     19
			     (exact->inexact
			      (+ 18 (/ (string->number value) 100))))))
	      ((branch) #f)
	      (else (botl-set! key (string->number value))))))
	(cond ((match regex str) => match-submatches)
	      (else (ret #f)))))
     (foreach-botl bot1 (get-row-plaintext 23))
     (foreach-botl bot2 (get-row-plaintext 24)))))
