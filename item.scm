(define table #f)

(define priest? #f)
(define (item-set-priest) (set! priest? #t))
(define samurai? #f)
(define (item-set-samurai) (set! samurai? #t))

(define (add-item-v name vec)
  (table-set! table name vec))

(define (add-item name type plural cost weight material appearance)
  (let ((vec (vector name type plural cost weight material appearance)))
    (add-item-v name vec)
    (if plural
	(add-item-v plural vec))
    (if (and appearance (not (string=? name appearance)))
	(add-item-v appearance vec))))

(define (make-item-alias a b)
  (add-item-v a (table-ref table b)))

(define (item-init)
  (set! table (make-string-table))
  (set! item-cache (make-vector cache-size))
  (set! submatch-cache (make-vector cache-size))
  (set! cursor 0)
  (set! cursor-prev (- cache-size 1))
  (amulets-init)
  (armor-init)
  (food-init)
  (gems-init)
  (potions-init)
  (rings-init)
  (scrolls-init)
  (spellbooks-init)
  (tools-init)
  (wands-init)
  (weapons-init)
  (if samurai? (japanese-init))
  
  (add-item "gold piece" 'gold "gold pieces" 1 1/100 #f "gold piece")
  (add-item "boulder" 'boulder/statue "boulders" 0 6000 #f "boulder")

  (make-item-alias "unlabeled scroll" "scroll of blank paper")
  (make-item-alias "unlabeled scrolls" "scrolls of blank paper")
  (make-item-alias "clear potion" "potion of water")
  (make-item-alias "clear potions" "potions of water")
  (make-item-alias "potion of holy water" "potion of water")
  (make-item-alias "potions of holy water" "potions of water")
  (make-item-alias "potion of unholy water" "potion of water")
  (make-item-alias "potions of unholy water" "potions of water"))

(define (item-slot item)
  (get-submatch
   item
   'slot
   (lambda (m)
     (string-ref item (match-start m)))))
		
(define (item-quantity item)
  (let ((x (get-submatch
	    item
	    'number
	    (lambda (m)
	      (let* ((str (and m (substring item
					    (match-start m)
					    (match-end m))))
		     (n (if (and str (char-numeric? (string-ref str 0)))
			    (string->number str)
			    1)))
		(cons n m))))))
    (and x (car x))))

(define (item-class item)
  (cond
   ((get-item-field item 1) => identity)
   ((item-corpse? item) 'food)
   ((member (item-name item)
	    '("bag" "lamp" "whistle" "horn" "candle" "candles" "harp"
	      "flute" "drum"))
    'tool)
   ((member (item-name item) '("scroll" "scrolls")) 'scroll)
   ((potion? item) 'potion)
   ((wand? item) 'wand)
   ((ring? item) 'ring)
   ((amulet? item) 'amulet)
   ((spellbook? item) 'spellbook)
   ((or (member (item-name item) '("boots" "gloves" "cloak"))
	(randomized-armor? item))
    'armor)
   ((unidentified-gem? item) 'gem)
   ((member (item-name item)
	    '("gray stone" "gray stones" "stone" "stones"))
    'stone)
   ((or (item-tin? item)
	(item-egg? item))
    'food)
   ((artifact? item) (item-class (artifact-base-item item)))
   ((item-figurine? item) 'tool)
   ((or (string=? (item-name item) "boulder") (item-statue? item))
    'statue/boulder)
   ((string=? (item-name item) "Amulet of Yendor") 'amulet)
   (else #f)))

(define (of-figurine/statue item figurine?)
  (let* ((name (item-name item))
	 (str (if figurine? "figurine" "statue"))
	 (mon (or (string-drop-prefix (string-append str " of a ") name)
		  (string-drop-prefix (string-append str " of an ") name))))
    (and mon
	 (monster-valid? mon)
	 mon)))

(define (item-figurine-of item) (of-figurine/statue item #t))
(define (item-statue-of item) (of-figurine/statue item #f))

(define (item-tin-of item)
  (let* ((name (item-name item))
	 (stuff (or (string-drop-prefix "tin of " name)
		    (string-drop-prefix "tins of " name)))
	 (mon (and stuff
		   (or (string-drop-suffix " meat" stuff)
		       stuff))))
    (and mon
	 (or (and (monster-valid? mon) mon)
	     (and (string=? mon "spinach") mon)))))

(define (of-egg/corpse item egg?)
  (let ((name (item-name item))
	(str (if egg? " egg" " corpse"))
	(len (if egg? 4 7)))
    (and (string-suffix? str name)
	 (let ((mon (string-drop-right name len)))
	   (and (monster-valid? mon)
		mon)))))

(define (item-egg-of item) (of-egg/corpse item #t))
(define (item-corpse-of item) (of-egg/corpse item #f))

(define (item-egg? item)
  (or (string=? (item-name item) "egg")
      (item-egg-of item)))
(define (item-tin? item)
  (or (member (item-name item)
	      '("tin" "tin of spinach" "empty tin"
		"tins" "tins of spinach" "empty tins"))
      (item-tin-of item)))

(define item-figurine? item-figurine-of)
(define item-statue? item-statue-of)
(define (item-historic-statue? item)
  (and (item-statue? item)
       (get-boolean-submatch item 'historic)))
(define item-corpse? item-corpse-of)

(define (item-price item) (get-numeric-submatch item 'price))
(define (item-recharges item) (get-numeric-submatch item 'recharges))
(define (item-charges item) (get-numeric-submatch item 'charges))
(define (item-enchantment item) (get-numeric-submatch item 'enchantment))

(define (item-diluted? item) (get-boolean-submatch item 'diluted))
(define (item-greased? item) (get-boolean-submatch item 'greased))
(define (item-poisoned? item) (get-boolean-submatch item 'poisoned))
(define (item-lit? item) (get-boolean-submatch item 'lit))
(define (item-we-laid? item) (get-boolean-submatch item 'laid-by-you))
(define (item-chained-to? item) (get-boolean-submatch item 'chained-to-you))
(define (item-quivered? item) (get-boolean-submatch item 'in-quiver))
(define (item-alt-weapon? item) (get-boolean-submatch item 'alt-weapon))
(define (item-partly-eaten? item) (get-boolean-submatch item 'partly-eaten))
(define (item-partly-used? item) (get-boolean-submatch item 'partly-used))
(define (item-fooproof? item) (get-boolean-submatch item 'proof))
(define (item-worn? item) (get-string-submatch item 'wearing))
(define (item-worn-left? item) (string-contains (item-worn? item) "left"))
(define (item-worn-right? item) (string-contains (item-worn? item) "right"))

(define (item-name item) (get-string-submatch item 'name))
(define (item-labeled item) (get-string-submatch item 'labeled))
(define (item-called item) (get-string-submatch item 'called))
(define (item-named item) (get-string-submatch item 'named))

(define (item-max-erosion item)
  (define (ero-level str)
    (cond ((not str) 0)
	  ((string-contains str "thoroughly") 3)
	  ((string-contains str "very") 2)
	  (else 1)))
  (max (ero-level (get-string-submatch item 'ero1))
       (ero-level (get-string-submatch item 'ero2))))

(define (item-buc item)
  (define (in) (item-name item))
  (cond
   ((get-submatch
     item
     'buc
     (lambda (m)
       (and m (let ((str (substring item (match-start m) (match-end m))))
		(cond ((string=? str "uncursed") 'uncursed)
		      ((string=? str "blessed") 'blessed)
		      (else 'cursed))))))
    => identity)
   (priest? 'uncursed)
   ((or (item-charges item)
	(and (item-enchantment item)
	     (or (eq? (item-class item) 'weapon)
		 (member (in)
			 '("unicorn horn" "pick-axe" "grappling hook"
			   "iron hook")))))
    'uncursed)
   ((member (in) '("potion of holy water" "potions of holy water"))
    'blessed)
   ((member (in) '("potion of unholy water" "potions of unholy water"))
    'cursed)
   (else #f)))

(define (get-item-field item n)
  (let ((e (table-ref table (item-name item))))
    (and e (vector-ref e n))))

(define (item-identity item)
  (cond
   ((artifact? item) (or (item-named item) (item-name item)))
   ((get-item-field item 0))
   ; common cases:
   ((and (tool? item)
	 (assoc (item-called item)
		'(("holding" . "bag of holding")
		  ("sack" . "sack")
		  ("wishy" . "magic lamp")
		  ("oil" . "oil lamp")
		  ("tin" . "tin whistle"))))
    => cdr)
   ((item-called item)
    => (lambda (name)
	 (or (item-identity (string-append name " " (item-name item)))
	     (item-identity (string-append (item-name item) " of " name)))))
   ((item-tin-of item) =>
    (lambda (mon)
      (string-append "tin of "
		     (if (monster-vegetarian? mon)
			 mon
			 (string-append mon " meat")))))
   ((item-egg-of item) => (lambda (mon) (string-append mon " egg")))
   ((or (item-figurine? item)
	(item-statue? item)
	(item-corpse? item))
    (item-name item))
   (else #f)))

(define (item-appearance item)
  (let ((name (item-name item)))
    (cond
     ((artifact? item) (item-appearance (artifact-base-item item)))
     ((get-item-field item 6))
     ((and (item-identity item)
	   (string=? (item-identity item) "potion of water"))
      "clear potion")
     ((and (or (unidentified-spellbook? item)
	       (unidentified-potion? item)
	       (unidentified-gem? item)
	       (unidentified-wand? item)
	       (unidentified-amulet? item)
	       (unidentified-ring? item)
	       (unidentified-gem? item))
	   (= (item-quantity item) 1))
      (item-name item))
     ((and (eq? (item-class item) 'stone)
	   (not (member name '("rock" "rocks"))))
      "gray stone")
     ((item-tin? item) "tin")
     ((item-egg? item) "egg")
     ((or (item-corpse? item)
	  (item-statue? item)
	  (item-figurine? item))
      name)
     ((tool? item)
      (any (lambda (str)
	     (and (string-contains name str)
		  str))
	   '("horn" "candle" "whistle" "drum" "bag" "harp" "flute" "lamp")))
     ((and (scroll? item) (item-labeled item))
      => (lambda (label)
	   (string-append "scroll labeled " label)))
     ((and (armor? item)
	   (member name '("conical hat" "cornuthaum" "dunce cap")))
      "conical hat")
     ((and (amulet? item)
	   (string-contains name "Yendor"))
      "Amulet of Yendor")
     (else #f))))

(define (item-plural-form item) (get-item-field item 2))
(define (item-cost item) (get-item-field item 3))
(define (item-weight item) (get-item-field item 4))
(define (item-material item) (get-item-field item 5))

(define (tool? item) (eq? (item-class item) 'tool))
(define (food? item) (eq? (item-class item) 'food))
(define (armor? item) (eq? (item-class item) 'armor))
(define (scroll? item) (eq? (item-class item) 'scroll))

(define (item-wielded? item)
  (or (let ((m (get-string-submatch item 'weapon)))
	(and m (not (string-contains m "other"))))
      (let ((m (get-string-submatch item 'wielded)))
	(and m (not (string-contains m "other"))))))
(define (item-wielded-offhand? item)
  (or (let ((m (get-string-submatch item 'weapon)))
	(and m (string-contains m "other")))
      (let ((m (get-string-submatch item 'wielded)))
	(and m (string-contains m "other")))))
(define (item-wielded-either? item)
  (or (get-string-submatch item 'weapon)
      (get-string-submatch item 'wielding)))

(define (item-nutrition item)
  (cond ((item-corpse-of item) => monster-nutrition)
	(else 0)))

; candles are rare. ignore.
(define (item-stackable? item)
  (or (memq (item-class item) '(potion scroll stone gem gold))
      (item-plural-form item)))

(define (item-add-to-stack a b)
  (and (item-stackable? b)
       (every (lambda (f)
		(equal? (f a) (f b)))
	      (list item-name item-buc item-greased? item-poisoned?
		    item-max-erosion item-fooproof? item-partly-used?
		    item-partly-eaten? item-enchantment))
       (item-adjust-quantity b (+ (item-quantity a)
				  (item-quantity b)))))

(define (item-adjust-quantity item n)
  (and (>= n 1) ; "1 daggers" is okay. we don't care.
       (item-quantity item)
       (let ((m (cdr (get-submatch item 'number #f))))
	 (string-append (substring item 0 (match-start m))
			(number->string n)
			(string-drop item (match-end m))))))


(define sq sequence)
(define sb submatch)
(define sp (set #\space))
(define tx text)
(define (maybe pattern) (repeat 0 1 pattern))
(define number (repeat 1 #f numeric))
(define (one-of-tx . ls) (apply one-of (map tx ls)))

; regexps stolen from TAEB

(define (mk-pats ls)
  (apply sq
	 (map (lambda (pattern)
		(maybe (sq pattern (repeat sp))))
	      ls)))

(define prefix
  (mk-pats
   (list (sq (sb 'slot (union alphabetic (set "#$"))) sp (set "+-"))
	 (sb 'number (one-of (one-of-tx "a" "an" "the") number))
	 (sb 'buc (one-of-tx "blessed" "uncursed" "cursed"))
	 (sb 'greased (tx "greased"))
	 (sb 'poisoned (tx "poisoned"))
	 (sb 'historic (tx "historic"))
	 (sb 'ero1 (sq (maybe (sq (one-of-tx "very" "thoroughly") sp))
		       (sq (one-of-tx "burnt" "rusty") sp)))
	 (sb 'ero2 (sq (maybe (sq (one-of-tx "very" "thoroughly") sp))
		       (sq (one-of-tx "rotted" "corroded") sp)))
	 (sb 'proof (one-of (tx "fixed")
			    (sq (one-of-tx "fire" "rust" "corrode")
				(tx "proof"))))
	 (sb 'partly-used (tx "partly used"))
	 (sb 'partly-eaten (tx "partly eaten"))
	 (sb 'diluted (tx "diluted"))
	 (sb 'enchantment (sq (set "+-") number))
	 (sq (one-of-tx "pair" "set") (tx " of")))))

(define suffix
  (mk-pats
   (list (sq (tx "labeled ") (sb 'labeled (repeat printing)))
	 (sq (tx "called ") (sb 'called (repeat printing)))
	 (sq (tx "named ") (sb 'named (repeat printing)))
	 (sq (tx "(")
	     (sb 'recharges number)
	     (tx ":")
	     (sb 'charges (sq (maybe (tx "-")) number))
	     (tx ")"))
	 (sq (tx "(")
		   (sq (sb 'ncandles (one-of (tx "no") (set "01234567"))))
		   (sq (tx " candle") (maybe (tx "s")) (tx ", "))
		   (sb 'candelabrum-lit (one-of-tx "lit" "attached"))
		   (tx ")"))
	       (sb 'lit (tx "(lit)"))
	       (sb 'laid-by-you (tx "(laid by you)"))
	       (sb 'chained-to-you (tx "(chained to you)"))
	       (sb 'in-quiver (tx "(in quiver)"))
	       (sb 'alt-weapon (tx "(alternate weapon; not wielded)"))
	       (sb 'wielded (sq (tx "(wielded")
				(maybe (tx " in other hand"))
				(tx ")")))
	       (sb 'weapon (sq (tx "(weapon in ")
			       (maybe (tx "other "))
			       (tx "hand)")))
	       (sb 'wearing (sq (tx "(")
				(one-of-tx "being" "embedded" "on")
				(repeat (subtract printing (set #\))))
				(tx ")")))
	       (sq (tx "(unpaid, ")
		   (sb 'price number)
		   (tx " zorkmid")
		   (maybe (tx "s"))
		   (tx ")")))))

(define item-cache #f)
(define submatch-cache #f)
(define cache-size 8)
(define cursor #f)
(define cursor-prev #f)

(define (cache-slot item)
  (let loop ((i cursor-prev))
    (cond ((= i cursor) #f)
	  ((eq? item (vector-ref item-cache i)) i)
	  (else (loop (if (= i 0) (- cache-size 1) (- i 1)))))))

(define (cache-item item submatches)
  (vector-set! item-cache cursor item)
  (vector-set! submatch-cache cursor submatches)
  (set! cursor-prev cursor)
  (set! cursor (if (= (+ cursor 1) cache-size) 0 (+ cursor 1))))

(define (recache n)
  (if (not (= n cursor-prev))
      (let ((item (vector-ref item-cache cursor-prev))
	    (submatches (vector-ref submatch-cache cursor-prev)))
	(vector-set! item-cache cursor-prev (vector-ref item-cache n))
	(vector-set! submatch-cache cursor-prev (vector-ref submatch-cache n))
	
	(vector-set! item-cache n item)
	(vector-set! submatch-cache n submatches))))

(define (match-item item)
  (let* ((pre-m (match (sq (string-start) prefix sp) item))
	 (item-start (if pre-m (match-end pre-m) 0))
	 (suf-m (match (sq sp suffix (string-end)) item))
	 (item-end (if suf-m (match-start suf-m) (string-length item)))
	 (name (string-trim-right (substring item item-start item-end)
				  #\space))
	 (subs (cons (cons 'name name)
		     (append (if pre-m (match-submatches pre-m) '())
			     (if suf-m (match-submatches suf-m) '())))))
    (cache-item item subs)))

(define (get-submatch item key f)
  (let* ((n (or (cache-slot item)
		(begin (match-item item)
		       cursor-prev)))
	 (submatches (vector-ref submatch-cache n))
	 (cell (assq key submatches))
	 (value (and cell (cdr cell))))
    (recache n)
    (and value
	 (if (match? value)
	     (begin (set-cdr! cell (f value))
		    (cdr cell))
	     value))))

(define (get-string-submatch item key)
  (get-submatch
   item
   key
   (lambda (m) (substring item (match-start m) (match-end m)))))

(define (get-boolean-submatch item key)
  (get-submatch item key (lambda (m) (not (not m)))))

(define (get-numeric-submatch item key)
  (get-submatch
   item
   key
   (lambda (m)
     (string->number (substring item (match-start m) (match-end m))))))