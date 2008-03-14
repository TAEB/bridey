(define table #f)

(define symbol-color-table #f)

(define last-mon #f)
(define last-vec #f)

(define (add-mon name symbol level speed ac mr alignment corpse? weight nutrition
		 size res res-conveyed flags color)
  (let* ((key (list symbol color))
	 (x (assoc key symbol-color-table)))
    (if (not x)
	(set! symbol-color-table (cons (list key name) symbol-color-table))
	(set-cdr! x (cons name (cdr x)))))
  (table-set! table
	      name
	      (vector symbol level speed ac mr alignment corpse? weight nutrition
		      size res res-conveyed flags color)))

(define (monster-get-field mon n)
  (let ((x (if (eq? mon last-mon)
	       last-vec
	       (table-ref table mon))))
    (set! last-mon mon)
    (set! last-vec x)
    (and x (vector-ref x n))))

(define (monster-valid? mon) (table-ref table mon))
(define (monster-symbol mon) (monster-get-field mon 0))
(define (monster-base-level mon) (monster-get-field mon 1))
(define (monster-speed mon) (monster-get-field mon 2))
(define (monster-ac mon) (monster-get-field mon 3))
(define (monster-mr mon) (monster-get-field mon 4))
(define (monster-alignment mon) (monster-get-field mon 5))
(define (monster-leaves-corpse? mon) (monster-get-field mon 6))
(define (monster-weight mon) (monster-get-field mon 7))
(define (monster-nutrition mon) (monster-get-field mon 8))
(define (monster-size mon) (monster-get-field mon 9))
(define (monster-resistances mon) (monster-get-field mon 10))
(define (monster-resistances-conveyed mon) (monster-get-field mon 11))
(define (monster-flags mon) (monster-get-field mon 12))
(define (monster-color mon) (monster-get-field mon 13))

(define (monster-breathless? mon) (memq 'breathless (monster-flags mon)))

(define (monster-vegan? mon)
  (member (monster-symbol mon) '(#\b #\j #\F)))

(define (monster-vegetarian? mon)
  (or (monster-vegan? mon)
      (and (char=? (monster-symbol mon) #\P)
	   (not (string=? mon "black pudding")))))

(define (monster-always-hostile? mon)
  (let ((flags (monster-flags mon)))
    (and flags
	 (not (or (memq 'not-always-hostile flags)
		  (memq 'peaceful flags))))))

(define (monster-always-peaceful? mon)
  (memq 'peaceful (monster-flags mon)))

(define (monster-race mon)
  (case (monster-symbol mon)
    ((#\G) 'gnome)
    ((#\o) 'orc)
    ((#\h) (let ((c (monster-color mon)))
	     (case (monster-color c)
	       ((red blue) 'dwarf)
	       ((magenta) (and (string=? mon "dwarf king") 'dwarf))
	       (else #f))))
    ((#\@) (if (member mon '("Woodland-elf" "Green-elf" "Grey-elf" "elf-lord"
			     "Elvenking"))
	       'elf
	       'human))))

(define (race-hostile? state mon)
  (and-let* ((mrace (monster-race mon)))
    (case (get-state state 'race)
      ((human) (memq mrace '(gnome orc)))
      ((elf dwarf) (eq? mrace 'orc))
      ((gnome) (eq? mrace 'human))
      ((orc) (memq mrace '(human elf dwarf)))
      (else #f))))

(define (race-peaceful? state mon)
  (and-let* ((mrace (monster-race mon)))
    (case (get-state state 'race)
      ((elf) (eq? mrace 'elf))
      ((dwarf gnome) (memq mrace '(dwarf gnome)))
      (else #f))))

(define (monster-peace-minded? state mon)
  (and (not (monster-always-hostile? mon))
       (not (race-hostile? state mon))
       (or (monster-always-peaceful? mon)
	   (race-peaceful? state mon)
	   (let* ((ual (case (get-state state 'alignment)
			 ((lawful) 1)
			 ((neutral) 0)
			 ((chaotic) -1)))
		  (mal (monster-alignment mon))
		  (same-align? (if (zero? ual)
				   (zero? mal)
				   (> (* ual mal) 0))))
	     (and same-align?
		  (not (and (get-state state 'have-aoy?)
			    (< mal 0))))))))
