(add-command far-look coord) ; the far-look command, ';'
;(add-command square-char coord) ; return character shown on the map at (x y)
(add-command look)
(add-command get-inventory)
(add-command engraving)
(add-command objects-here)

;; (add-command hp)
;; (add-command str)
;; (add-command dex)
;; (add-command con)
;; (add-command int)
;; (add-command wis)
;; (add-command cha)
;; (add-command dlvl)
;; (add-command xplvl)
;; (add-command xp)
;; (add-command turn-count)

(add-command get-raw-output)

;(add-command blind?)
(define (blind?) #f)

(add-command hostile? monster)


; the item functions...
(add-command quantity item)
(add-command buc item)
(add-command greased? item)
(add-command errodeproof? item)
(add-command errosion-level item)
(add-command enchantment item)
(add-command item-name item)
(add-command item-group-name item)
(add-command item-indiv-name item)
(add-command unpaid? item)
(add-command cost item)
(add-command wielded? item)
(add-command wielded-oh? item)
(add-command quivered? item)
(add-command alternate-weapon? item)
; TODO partly eaten, embedded in skin, welded to hand, etc. etc.

(define (move state dir . opt)
  (define get (specialize get-state state))
  (define set (specialize set-state state))
  (let ((old-coord (get-coord))
	(dest (map + (get-coord) dir))
	(command (list 'move dir))
	(step-on-brown-+? (and (not (null? opt)) (car opt))))
    (cond ((or (and (door? state dest)
		    (diagonal? dir))
	       (and (char=? (square-char dest) #\+)
		    (eq? (square-color dest) 'brown)
		    (not step-on-brown-+?)))
	   (do-door state dir))
	  ((get 'in-pit?)
	   (ask (list 'move (dir->vi dir)))
	   (process-turn (set 'last-command command
			      'expected-coord old-coord)))
	  (else
	   (ask (list 'move (dir->vi dir)))
	   (term-process (get-raw-output))
	   (if (not (equal? (get-coord) old-coord))
	       (process-turn
		(set 'last-coord old-coord
		     'last-commad command
		     'expected-coord (map + old-coord dir)))
	       (cond ((and (diagonal? dir)
			   (not (square-clear? state dest)))
		      (do-door state dir))
		     ((item? state coord)
		      (let ((str (cadr (far-look dest))))
			(cond ((or (string-suffix? " embedded in stone" str)
				   (string-suffix? " embedded in a wall" str))
			       (mark-embedded state dest))
			      ((string-suffix? " embedded in a door" str)
			       (mark-embedded state dest)
			       (mark-door state dest))
			      (else
			       (display "move: can't move into object\n")))
			(process-turn
			 (set 'last-command command
			      'expected-coord old-coord))))))))))

  
(define (fight state dir)
  (ask (list 'fight (dir->vi dir)))
  (term-process (get-raw-output))
  (process-turn
   (set-state state
	      'last-command (list 'fight dir)
	      'expected-coord (get-state state 'coord))))

(define (kick state dir)
  (if (get-state state 'injured?)
      (search state)
      (begin
	(ask (list 'kick (dir->vi dir)))
	(term-process (get-raw-output))
	(process-turn
	 (set-state state
		    'last-command (list 'kick dir)
		    'expected-coord (get-state state 'coord))))))

(define (open-door dir)
  (ask (list 'open-door (dir->vi dir)))
  (term-process (get-raw-output))
  (member "The door opens." (get-msgs)))

(define (close-door dir)
  (ask (list 'close-door (dir->vi dir)))
  (term-process (get-raw-output))
  (member "The door closes." (get-msgs)))

(define (search state)
  (let ((walls (filter wall? (neighbor-squares))))
    (ask '(search))
    (term-process (get-raw-output))
    (for-each (lambda (c)
		(if (char=? (square-char c) #\+)
		    (mark-door state c)))
	      walls)
    (map-bv-modify! (get-state state 'searched) (get-coord) (specialize + 1))
    (process-turn
     (set-state state
		'last-command '(search)
		'expected-coord (get-state state 'coord)))))

(define (pick-up state . ls)
  (let ((command (cons 'pick-up ls)))
    (ask command)
    (term-process (get-raw-output))
    (process-turn (set-state state
			     'last-command command
			     'expected-coord (get-state state 'coord)))))

(define (get-msgs)
  (filter (lambda (s) (not (string=? s ""))) (ask '(get-msgs))))

(define (old-square-char coord) (ask (list 'square-char coord)))
(define (old-get-row-plaintext y) (ask (list 'old-get-row-plaintext y)))
(define (old-get-coord) (ask '(get-coord)))

(add-command get-room-extents)
(add-command save)

; Turns control over to the terminal running the perl script, so
; I can play manually.  The 'v' key is intercepted as the toggle
; back to bot-control.
(add-command to-user)

(add-command split-inventory-item str)
(add-command inventory-item? str)


(add-command want-item? str)
