;(add-command far-look coord) ; the far-look command, ';'
;(add-command look)
;(add-command get-inventory)
;(add-command engraving)
;(add-command objects-here)

;(add-command blind?)
(define (blind?) #f)

;(add-command hostile? monster)


; the item functions...
;; (add-command quantity item)
;; (add-command buc item)
;; (add-command greased? item)
;; (add-command errodeproof? item)
;; (add-command errosion-level item)
;; (add-command enchantment item)
;; (add-command item-name item)
;; (add-command item-group-name item)
;; (add-command item-indiv-name item)
;; (add-command unpaid? item)
;; (add-command cost item)
;; (add-command wielded? item)
;; (add-command wielded-oh? item)

;; (add-command quivered? item)
;; (add-command alternate-weapon? item)
; TODO partly eaten, embedded in skin, welded to hand, etc. etc.

(define (move state dir . opt)
  (define get (specialize get-state state))
  (define set (specialize set-state state))
  (let ((old-coord (get-coord))
	(dest (map + (get-coord) dir))
	(command (list 'move dir))
	(step-on-brown-+? (and (not (null? opt)) (car opt))))
    (cond ((or (locked-door? dest)
	       (closed-door? dest)
	       (and (door? dest)
		    (diagonal? dir))
	       (and (char=? (square-char dest) #\+)
		    (eq? (square-color dest) 'brown)
		    (not step-on-brown-+?)))
	   (do-door state dir))
	  ((get 'in-pit?)
	   (send-expect (dir->vi dir) expect-no-change)
	   (process-turn (set 'last-command command
			      'expected-coord old-coord)))
	  (else
	   (send-expect
	    (dir->vi dir)
	    (lambda (res tries)
	      (or (and (equal? (get-coord) dest)
		       (get 'avatar)
		       (char=? (square-char (get-coord)) (get 'avatar))
		       (botl-update)
		       (or (not (char=? (square-char '(1 1)) #\space))
			   (> tries 2)))
		  (> tries 4))))
	   (if (not (equal? (get-coord) old-coord))
	       (process-turn
		(set 'last-coord old-coord
		     'last-command command
		     'expected-coord (map + old-coord dir)))
	       ; okay, we didn't move. Make sure we're done reading, then
	       ; investigate.
	       (begin
		 (read-expect (lambda (res tries) (> tries 2)))
		 (cond ((or (and (diagonal? dir)
				 (not (square-clear? state dest)))
			    (open-door? dest)) ; maybe a monster closed it
			(do-door state dir #t))
		       ((item? state dest)
			(let ((str (far-look dest)))
			  (cond ((or (string-suffix? " embedded in stone" str)
				     (string-suffix? " embedded in a wall" str))
				 (mark-embedded dest))
				((string-suffix? " embedded in a door" str)
				 (mark-embedded dest)
				 (mark-closed-door dest))
				(else
				 (display "move: can't move into object\n")))
			  (process-turn
			   (set 'last-command command
				'expected-coord old-coord))))
		       (else (display "can't move\n")))))))))
  
(define (fight state dir)
  (send-expect (string-append "F" (dir->vi dir)) expect-generic)
  (process-turn
   (set-state state
	      'last-command (list 'fight dir)
	      'last-coord (get-state state 'coord)
	      'expected-coord (get-state state 'coord))))

; TODO: make more robust.
(define (do-door state dir . opt)
  (define get (specialize get-state state))
  (define set (specialize set-state state))
  (define (wall-in-dir? dir) (wall? (map + (get-coord) dir)))
  (define (get-aligned)
    (let* ((dir1 (list (car dir) 0))
	   (dir2 (list (cadr dir) 0))
	   (sqr1 (map + (get-coord) dir1))
	   (sqr2 (map + (get-coord) dir2))
	   (bad? (lambda (c) (or (wall? c) (bad-trap? state c)))))
      (if (not (bad? sqr1))
	  (move state dir1)
	  (and (not (bad? sqr2))
	       (move state dir2)))))
  (let ((coord (map + (get-coord) dir))
	(char (square-char-dir dir))
	(update? (and (not (null? opt)) (car opt))))
    (cond ((and (open-door? coord) (not update?))
	   (if (diagonal? dir)
	       (get-aligned)
	       (move state dir)))
	  ((and (locked-door? coord) (not update?))
	   (if #f ; have key
	       'unlock
	       (if (diagonal? dir)
		   (get-aligned)
		   (if (> (nchars-identical (get 'engraving)
					    "Closed for inventory.")
			  4)
		       'make-a-note-somewhere
		       (kick state dir)))))
	  ((not (or (and (char=? char #\+)
			 (eq? (square-color coord) 'brown))
		    (diagonal? dir)
		    (embedded? coord)))
	   (move state dir))
	  (else
	   (send-expect
	    (string-append "o" (dir->vi dir))
	    (lambda (res tries)
	      (call/cc
	       (lambda (return)
		 (cond
		  ; these don't take a turn, so there can't be other messages
		  ((term-match-string? "This door is locked." '(1 1))
		   (mark-locked-door coord))
		  ((or (term-match-string? "This doorway has no door." '(1 1))
		       (term-match-string? "This door is broken." '(1 1))
		       (term-match-string? "You see no door there." '(1 1)))
		   (unmark-door coord))
		  ((term-match-string? "This door is already open." '(1 1))
		   (mark-open-door coord))
		  ((expect-generic res tries) #t)
		  (else (return #f)))
		 #t))))
	   (process-turn
	    (set 'last-command (list 'open-door dir)
		 'last-coord (get 'coord)
		 'expected-coord (get 'coord)))))))

(define (kick state dir)
  (if (get-state state 'injured?)
      (search state)
      (let ((state
	     (send-expect
	      (string (integer->char 4)) ; ^D
	      (lambda (res tries)
		(or (and (at-more?) ; injured
			 state)
		    (and (match-before-cur? "In what direction? ")
			 (send-expect (dir->vi) expect-generic)
			 state))))))
	(process-turn
	 (set-state state
		    'last-command (list 'kick dir)
		    'last-coord (get-state state 'coord)
		    'expected-coord (get-state state 'coord))))))

(define (search state)
  (let ((walls (filter wall? (neighbor-squares))))
    (send-expect "s" expect-no-change)
    (for-each (lambda (c)
		(if (char=? (square-char c) #\+)
		    (mark-closed-door c)))
	      walls)
    (map-bv-modify! (get-state state 'searched) (get-coord) (specialize + 1))
    (if (not (get-state state 'blind?))
	(for-each mark-seen (neighbor-squares (get-coord))))
    (process-turn
     (set-state state
		'last-command '(search)
		'last-coord (get-state state 'coord)
		'expected-coord (get-state state 'coord)))))

(define (wait state)
  (send-expect "." expect-no-change)
  (process-turn
   (set-state state
	      'last-command '(wait)
	      'last-coord (get-state state 'coord)
	      'expected-coord (get-state state 'coord))))

;; (define (pick-up state . ls)
;;   (let ((command (cons 'pick-up ls)))
;;     (ask command)
;;     (term-process (get-raw-output))
;;     (process-turn (set-state state
;; 			     'last-command command
;; 			     'expected-coord (get-state state 'coord)))))

;(add-command save)

;(add-command want-item? str)

