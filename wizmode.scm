(define locations #f)

(define location-names
  '(("Stair to The Gnomish Mines" .  mines)
    ("oracle" . oracle)
    ("Stair to Sokoban" . soko)
    ("bigrm" . bigroom)
    ("Portal to The Quest" . quest)
    ("rogue" . rogue)
    ("medusa" . medusa)
    ("castle" . castle)
    ("valley" . votd)
    ("asmodeus" . asmodeus)
    ("baalz" . baalzebub)
    ("juiblex" . juiblex)
    ("Stair to Vlad's Tower" . vlad)
    ("orcus" . orcus)
    ("wizard3" . wizard-bottom)
    ("fakewiz1" . fake-wizard-1)
    ("fakewiz2" . fake-wizard-2)))

(define (wizmode-get-locations)
  (send-expect (char->control-string #\O) expect-more)
  (let loop ((i 2)
	     (ls '()))
    (if (= i 24) ; all on first page
	(begin
	  (send-expect "\e" expect-generic)
	  (set! locations ls))
	(let* ((str (get-row-plaintext i))
	       (colon (string-index str #\:))
	       (name (string-trim-both (substring str 0 colon)))
	       (level (string->number
		       (string-trim-right (string-drop str (+ colon 2))))))
	  (loop (+ i 1)
		(if (assoc name location-names)
		    (cons (cons (cdr (assoc name location-names)) level)
			  ls)
		    ls))))))

(define (wizmode-location loc)
  (if (not locations)
      (wizmode-get-locations))
  (cdr (assq loc locations)))

(define (wizmode-map)
  (send-expect (char->control-string #\F) expect-dunno)
  (mark-all-corridors-seen))

(define (wizmode-levelport level)
  (send-expect (char->control-string #\V)
	       (lambda (res tries)
		 (match-before-cur? "To what level do you want to teleport? ")))
  (send-expect (string-append (number->string level) "\n")
	       expect-dunno))

(define (wizmode-wish str)
  (send-expect (char->control-string #\W)
	       (lambda (res tries)
		 (match-before-cur? "For what do you wish? ")))
  (send-expect (string-append  str "\n")
	       expect-generic))

(define (wizmode-create-monster str)
  (send-expect (char->control-string #\G)
	       (lambda (res tries)
		 (match-before-cur? "[type the name or symbol] ")))
  (send-expect (string-append  str "\n")
	       expect-generic))

(define (wizmode-identify-inventory)
  (send-expect (char->control-string #\I)
	       expect-generic)
  (let loop ()
    (if (at-more?)
	(begin (send-expect " " expect-generic)
	       (loop)))))

(define last-checked -1)
(define align #f)
(define hunger #f)

(define (update-align-hunger)
  (if (not (= last-checked (turns)))
      (begin
	(send-expect (char->control-string #\X) expect-menu)
	(send-expect " " expect-more)
	(let* ((x (car (get-coord)))
	       (y (cadr (get-coord)))
	       (hun-str "Your hunger is ")
	       (aln-str "Your alignment is ")
	       (start-col (- x 8)))
	  (let loop ((row 3))
	    (let ((str (get-row-plaintext row start-col)))
	      (if (string-prefix? hun-str str)
		  (set! hunger
			(string->number
			 (substring str
				    (string-length hun-str)
				    (string-index str #\.)))))
	      (if (string-prefix? aln-str str)
		  (set! align
			(string->number
			 (substring str
				    (string-length aln-str)
				    (string-index str #\.))))))
	    (if (< row y)
		(loop (+ row 1)))))
	(send-expect " " expect-generic)
	(set! last-checked (turns)))))


(define (wizmode-get-hunger)
  (begin (update-align-hunger)
	 hunger))

(define (wizmode-get-align)
  (begin (update-align-hunger)
	 align))
  
	    
