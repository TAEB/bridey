(define nao? #f)

(define send #f)
(define send-expect #f)
(define read-expect #f)

(define (char-selection char)
  (let ((role (case (car char)
		((archeologist) "a") ((barbarian) "b") ((caveman) "c")
		((healer) "h") ((knight) "k") ((monk) "m")
		((priest) "p") ((rogue) "r") ((ranger) "R")
		((samurai) "s") ((tourist) "t") ((valkyrie) "v")
		((wizard) "w")))
	(race (case (cadr char)
		((human) "h") ((elf) "e") ((dwarf) "d")
		((gnome) "g") ((orc) "o")))
	(sex (case (caddr char)
	       ((male) "m") ((female) "f")))
	(alignment (case (cadddr char)
		     ((lawful) "l") ((neutral) "n") ((chaotic) "c"))))
    (send-expect role expect-generic)
    (if (string-contains (get-row-plaintext 1) "Pick the race")
	(send-expect race expect-generic))
    (if (string-contains (get-row-plaintext 1) "Pick the gender")
	(send-expect sex expect-generic))
    (if (string-contains (get-row-plaintext 1) "Pick the alignment")
	(send-expect alignment expect-generic))
    (send-expect " " expect-generic)))

(define (nethack-init char)
  (if nao?
      (begin (set! read-expect telnet-read-expect)
	     (set! send-expect telnet-send-expect)
	     (telnet-init))
      (begin (set! read-expect pty-read-expect)
	     (set! send-expect pty-send-expect)
	     (pty-init)))
  (read-expect
   (lambda (res tries)
     (or (expect-generic res tries)
	 (and (string-prefix? "There are some stale nethack processes"
			      (get-row-plaintext 3))
	      (system "sleep 10")
	      #f))))
  (cond ((match-before-cur? "for you? [ynq] ")
	 (send-expect "n" expect-menu)
	 (char-selection char))
	((match-before-cur? "Restoring save file...--More--")
	 (send-expect " " expect-generic))
	(else 'startup-error)))

(define (nethack-end)
  (if nao?
      (telnet-end)
      (pty-end)))

(define (nethack-redraw)
  (term-init)
  (send-expect (char->control-string #\R) expect-dunno))
