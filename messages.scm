(define table #f)

(define num-entries 0)

(define-syntax match
  (syntax-rules ()
    ((match state get set ((string ...) body ...))
     (let ((ls '(string ...))
	   (f (lambda (state)
		(let ((get (specialize get-state state))
		      (set (specialize set-state state)))
		  body ...))))
       (for-each (lambda (s)
		   (table-set! table s f)
		   (set! num-entries (+ num-entries 1)))
		 ls)))
    ((match state get set clause1 clause2 ...)
     (begin
       (match state get set clause1)
       (match state get set clause2 ...)))))

(define (populate)
  (define (state-mark-trap state type)
    (let ((expected (get-state state 'expected-coord)))
      (if (not (equal? (get-coord) expected))
	  (begin
	    (mark-trap expected 'maybe)
	    (set-state state 'do-look? #t))
	  (begin
	    (mark-trap expected type)
	    state))))
  (match
   state
   get
   set
   (("The door opens.")
    (if (same-level? state)
	(let ((c (map + (get 'expected-coord) (cadr (get 'last-command)))))
	  (mark-open-door c)
	  (unmark-embedded c)))
    state)
   (("The door resists!")
    (if (same-level? state)
	(mark-closed-door
	 (map + (get 'expected-coord) (cadr (get 'last-command)))))
    state)
   (("As you kick the door, it crashed open!"
     "You kick at empty space.")
    (let ((c (map + (get 'expected-coord) (cadr (get 'last-command)))))
      (unmark-door c)
      (unmark-embedded c))
    state)
   (("There is an open door here.")
    (if (weird-position? state)
	(set 'do-look? #t)
	(begin (mark-open-door (get-coord))
	       state)))
   
   (("There is a fountain here.")
    (if (weird-position? state)
	(set 'do-look? #t)
	(if (fountain? (get-coord))
	    state
	    (begin (add-fountain (get-coord))
		   (send-event state 'found-fountain (get-coord))))))
   (("There is an opulent throne here.")
    (if (weird-position? state)
	(set 'do-look? #t)
	(if (throne? (get-coord))
	    state
	    (begin (add-throne (get-coord))
		   (send-event state 'found-throne (get-coord))))))

   (("There is a doorway here.")
    state) ; TODO

   (("There are several objects here." "There are several more objects here."
     "There are many objects here." "There are many more objects here.")
    (if (weird-position? state)
	(set 'do-look? #t)
	(set 'maybe-pickup? #t)))

   (("You see no objects here." "You feel no objects here.")
    (if (weird-position? state)
	(set 'do-look? #t)
	(set 'objects-here '())))

   (("Something is written here in the frost."
     "Something is written here in the dust.")
    (if (weird-position? state)
	(set 'do-look? #t)
	(set 'engraving-type 'dust)))

   (("Something is engraved here on the floor."
     "Something is engraved here on the ground."
     "Something is engraved here on the ice."
     "Something is engraved here on the bridge."
     "Something is engraved here on the headstone.")
    (if (weird-position? state)
	(set 'do-look? #t)
	(set 'engraving-type 'engrave)))
     
   (("Some text has been burned into the floor here."
     "Some text has been burned into the ground here."
     "Some text has been burned into the bridge here."
     "Some text has been melted into the ice here.")
    (if (weird-position? state)
	(set 'do-look? #t)
	(set 'engraving-type 'burn)))

   (("The fountain dries up!"
     "As the hand retreats, the fountain disappears!")
    (remove-fountain (get 'expected-coord))
    state)

   (("The flow reduces to a trickle."
     "A watchman yells:  \"Hey, stop using that fountain!\""
     "A watch captain yells:  \"Hey, stop using that fountain!\"")
    (fountain-got-warning (get 'expected-coord))
    state)
     
   (("The water glows for a moment."
     "A feeling of loss comes over you.")
    ;; modify item
    state)

   (("You attract a water nymph!"
     "You hear a seductive voice.")
    ;; should mark summoned nymph as awake
    state)

   (("You spot a gem in the sparkling waters!"
     "You feel a gem here.")
    state)
   
   (("You feel transparent."
     "You feel very self-conscious."
     "You see an image of someone stalking you.")
    ;; see invisible. check last-command for foutain quaffing
    state)

   (("This water's no good!")
    (set 'do-inventory? #t))

   (("The boulder falls apart.")
    state)

   
   ;; status messages
   (("You feel confused." "Huh, What?" "You feel somewhat dizzy."
     "You feel trippy." "You feel rather light headed.")
    (set 'confused? #t))
   (("A cloud of darkness falls upon you." "You can't see any more."
     "It suddenly gets dark." "You are blinded by the flash!"
     "The raven blinds you!")
    (set 'blind? #t))
   (("Everything looks so cosmic!")
    (set 'hallu? #t))

   (("You feel less confused now.")
    (set 'confused? #f))
   (("You can see again.")
    (set 'blind? #f))
   (("Everything looks SO boring now.")
    (set 'hallu? #f))
   (("Everything feels SO boring now.")
    (set 'hallu? #f))
   (("You feel a bit steadier now.")
    (set 'stunned? #f))

   ;; intrinsics
   ;; TODO: send an event
   (("You feel a momentary chill." "You be chillin'.")
    (cons-state state 'intrinsics 'fire))
   (("You feel wide awake.")
    (cons-state state 'intrinsics 'sleep))
   (("You feel full of hot air.")
    (cons-state state 'intrinsics 'cold))
   (("You feel firm." "You feel totally together, man.")
    (cons-state state 'intrinsics 'disintegration))
   (("Your health currently feels amplified!"
     "You feel grounded in reality.")
    (cons-state state 'intrinsics 'shock))
   (("You feel healthy.")
    (cons-state state 'intrinsics 'poison))
   
   (("You feel very jumpy." "You feel diffuse.")
    (cons-state state 'intrinsics 'teleportitis))
   (("You feel in control of yourself."
     "You feel centered in your personal space.")
    (cons-state state 'intrinsics 'teleport-control))
   (("You feel a strange mental acuity."
     "You feel in touch with the cosmos.")
    (cons-state state 'intrinsics 'telepathy))
   (("You feel hidden!")
    (set 'intrinsics (append '(see-invisible invisibility)
			     (get 'intrinsics))))
   (("You seem faster." "You speed up." "Your quickness feels more natural.")
    (cons-state state 'intrinsics 'speed))

   ;; these below should call something besides just removing intrinsic
   ;; from 'intrinsics
   (("You feel warmer.")
    state)
   (("You feel less jumpy.")
    state)
   (("You feel a little sick!")
    state)
   (("Your senses fail!")
    state)
   (("You feel cooler.")
    state)
   (("You feel paranoid.")
    state)
   (("You thought you saw something!" "You tawt you taw a puttie tat!")
    state)
   (("You seem slower." "You feel slower." "Your quickness feels less natural.")
    state)
   (("You feel clumsy.")
    state)
   (("You feel vulnerable.")
    state)
   (("You feel less attractive.")
    state)

   ;; ID stuff
   (("You feel especially healthy.")
    state)
   (("You feel mildly chilly.")
    state)
   (("You feel mildly hot.")
    state)
   (("You feel mildly warm.")
    state)

   (("You feel like a new man!"
     "You feel like a new woman!"
     "You feel like a new gnome!"
     "You feel like a new elf!"
     "You feel like a new orc!"
     "You feel like a new dwarf!")
    ;; adjust stats
    state)
     


   ;; Temples
   (("You have a forbidding feeling."
     "You have a strange forbidding feeling."
     "You experience a strange sense of peace.")
    state)

   ;; Sounds
   (("You hear someone cursing shoplifters."
     "You hear the chime of a cash register."
     "You hear Neiman and Marcus arguing!")
    state)
   (("You hear a slow drip." "You hear a gurgling noise."
     "You hear dishes being washed!")
    state)
   (("You hear the tones of courtly conversation."
     "You hear a sceptre pounded in judgment."
     "Someone shouts \"Off with his head!\""
     "Someone shouts \"Off with her head!\""
     "You hear Queen Beruthiel's cats!")
    state)
   (("You hear mosquitos!" "You smell marsh gas!" "You hear Donald Duck!")
    state)
   (("You hear someone counting money."
     "You hear the footsteps of a guard on patrol."
     "You hear the quarterback calling the play."
     "You hear Ebenezer Scrooge!")
    state)
   (("You hear a sound reminiscent of an elephant stepping on a peanut." 
     "You hear a sound reminiscent of a seal barking." 
     "You hear Doctor Doolittle!")
    state)
   (("You hear a low buzzing." "You hear an angry drone.")
    state)
   (("You suddenly realize it is unnaturally quiet."
     "The hair on the back of your neck stands up."

     "The hair on your head seems to stand up."
     "You have an uncanny feeling..."
     "Run away!")
    state)
   (("You hear blades being honed."
     "You hear loud snoring."
     "You hear dice being thrown."
     "You hear General MacArthur!")
    state)
   (("You hear a strange wind."
     "You hear convulsive ravings."
     "You hear snoring snakes."
     "You hear someone say \"No more woodchucks!\""
     "You hear a loud ZOT!")
    state)

   (("You enter what seems to be an older, more primitive world.")
    (mark-special 'rogue)
    state)

   (("The heat and smoke are gone.")
    state)
   (("You penetrated a high security area!")
    state)

   ;; Nasty stuff
   (("You feel feverish.")
    state)
   (("You murderer!")
    state)
   (("You see an angry guard approaching!"
     "You hear the shrill sound of a guard's whistle.")
    ;; "Oh, shit."
    state)

   (("You wake up.")
    state)

   (("You feel limber!"
     "What a pity - you just ruined a future piece of art!"
     "What a pity - you just ruined a future piece of fine art!")
    state)

   
   (("You strain a muscle." "Your right leg is in no shape for kicking.")
    (set 'injured? #t))
   (("Your leg feels somewhat better.") (set 'injured? #f))
   (("You enter what seems to be an older, more primitive world.")
    (set 'rogue-level (dlvl)))

   ;; Traps
   (("You are momentarily blinded by a flash of light!"
     "You hear a deafening roar!")
    ;; TODO: evasive action... or something
    (state-mark-trap state 'magic))
   (("There is a magic trap here." "You escape a magic trap."
     "You see a flash of light!" "A shiver runs up and down your spine!"
     "You hear distant howling." "You hear the moon howling at you." ; hallu
     "Your pack shakes violently!"
     "You smell charred flesh." "You smell hamburgers.") ; hallu
    (state-mark-trap state 'magic))
   
   (("You triggered your land mine!" "You triggered the land mine!")
    (state-mark-trap (set 'injured? #t) 'pit))
   (("You fall into a pit!" "You fall into your pit!" "There is a pit here.")
    (state-mark-trap (set 'in-trap? #t) 'pit))
   (("You land on a set of sharp iron spikes!" "There is a spiked pit here.")
    (state-mark-trap state 'spiked-pit))
   (("You crawl to the edge of the pit.") (set 'in-trap? #f))
   (("There's a gaping hole under you!")
    (mark-trap (get 'expected-coord))
    (set 'traps (assoc-replace (list (get 'expected-coord) 'hole)
			       (get 'traps))))
   (("A trap door opens under you!")
    (mark-trap (get 'expected-coord))
    (set 'traps (assoc-replace (list (get 'expected-coord) 'hole)
			       (get 'hole))))
   (("You escape a trap door." "There is a trap door here.")
    (state-mark-trap state 'trap-door))
   (("You escape a hole." "You escape your hole."
     "You float over a hole." "You float over your hole."
     "You fly over a hole." "You fly over your hole."
     "There is a hole here.")
    (state-mark-trap state 'hole))
   (("You escape a squeaky board." "There is a squeaky board here."
     "You notice a loose board below you."
     "You notice a crease in the linoleum.") ; hallu
    (state-mark-trap state 'squeaky-board))
   (("You feel your magical energy drain away!"
     "There is an anti-magic field here.")
    (state-mark-trap state 'anti-magic))
   (("You escape a fire trap." "There is a fire trap here.")
    (state-mark-trap state 'fire))
   (("A little dart shoots out at you!"
     "You escape a dart trap." "There is a dart trap here.")
    (state-mark-trap state 'dart))
   (("An arrow shoots out at you!"
     "You escape an arrow trap." "There is an arrow trap here.")
    (state-mark-trap state 'arrow))
   (("A trap door in the ceiling opens and a rock falls on your head!"
     "There is a falling rock trap here.")
    (state-mark-trap state 'falling-rock))
   (("A cloud of gas puts you to sleep!"
     "There is a sleeping gas trap here." "You escape a sleeping gas trap."
     "You are enveloped in a cloud of gas!")
    (state-mark-trap state 'sleeping-gas))
   (("A bear trap closes on your foot!" "Your bear trap closes on your foot!")
    (state-mark-trap (set-state 'in-trap? #t) 'beartrap))
   (("You escape a bear trap." "There is a bear trap here."
     "A bear trap closes harmlessly over you."
     "Your bear trap closes harmlessly over you."
     "You float over the bear trap." "You float over the bear trap."
     "You fly over the bear trap." "You fly over your bear trap.")
    (state-mark-trap state 'beartrap))
   (("You escape a land mine." "You escape your land mine."
     "There is a land mine here."
     "There is a trigger in a pile of soil below you."
     "There is the trigger of your mine in a pile of soil below you."
     "You discover a trigger in a pile of soil below you."
     "You discover the trigger of your mine in a pile of soil below you.")
    (state-mark-trap state 'land-mine))
   (("You hear a loud click!" "You hear a soft click."
     "A trap door in the ceiling opens, but nothing falls out!"
     "Fortunately for you, no boulder was released."
     "You are caught in a magical explosion!")
    (unmark-trap (get 'expected-coord))
    state)))

(define (process-message msg state)
  (define get (specialize get-state state))
  (define set (specialize set-state state))
  (if (not table)
      (begin
	(set! table (make-string-table))
	(populate)))
  (cond
   ((table-ref table msg)
    => (lambda (f)
	 (f state)))
   ((inventory-item? msg)
    (cons-state state 'inventory (split-inventory-item msg)))
   ((string-drop-prefix "You kill the " msg)
    => (lambda (mon)
	 (maybe-add-corpse state (chop-punct mon))))
   ((or (string-drop-prefix "You see here " msg)
	(string-drop-prefix "You feel here " msg))
    => (lambda (item)
	 (if (weird-position? state)
	     (set 'do-look? #t)
	     (set 'objects-here (list (chop-punct item))))))
   ((or (string-drop-prefix "You read: \"" msg)
	(string-drop-prefix "You feel the words: \"" msg))
    => (lambda (str)
	 (let ((engraving (string-drop-suffix "\"." str)))
	   (if (and engraving
		    (not (weird-position? state)))
	       (set 'engraving engraving)
	       (set 'do-look? #t)))))
   ((string-drop-prefix "You finish eating " msg)
    => (lambda (food)
	 (let* ((item (chop-punct food))
		(nutrition (item-nutrition item)))
	   (if (eq? (car (get 'last-command)) 'eat-from-floor)
	       (set 'nutrition nutrition
		    'objects-here (delete-first
				   (lambda (x)
				     (string=? (item-name item)
					       (item-name x)))
				   (get 'objects-here))
		    'corpses (if (item-corpse? item)
				 (remove (lambda (x)
					   (and (equal? (get-coord) (car x))
						(string=? (item-corpse-of item)
							  (cadr x))))
					 (get 'corpses))
				 (get 'corpses)))
	       (decrement-item
		(set 'nutrition nutrition)
		(cadr (get 'last-command)))))))
   ((and (string-drop-suffix " here." msg)
	 (string-drop-prefix "There is an altar to " msg))
    => (lambda (str)
	 (let ((align (cond ((string-contains str "lawful") 'lawful)
			    ((string-contains str "neutral") 'neutral)
			    ((string-contains str "chaotic") 'chaotic)
			    ((string-contains str "unaligned") 'unaligned))))
	   (begin (add-altar (get-coord) align)
		  state))))
   (else state)))


