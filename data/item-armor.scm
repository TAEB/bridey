(define (randomized-cloak? item)
  (member (item-name item)
	  '("piece of cloth" "opera cloak" "ornamental cope" "tattered cape")))

(define (randomized-gloves? item)
  (member (item-name item)
	  '("old gloves" "padded gloves" "riding gloves" "fencing gloves")))

(define (randomized-helmet? item)
  (member (item-name item)
	  '("plumed helmet" "etched helmet" "crested helmet" "visored helmet")))

(define (randomized-boots? item)
  (member (item-name item)
	  '("mud boots" "buckled boots" "riding boots" "snow boots"
	    "hiking boots" "combat boots" "jungle boots")))

(define (randomized-armor? item)
  (or (randomized-cloak? item)
      (randomized-gloves? item)
      (randomized-helmet? item)
      (randomized-boots? item)
      (string=? (item-name item) "conical hat")))

(define (add-reg-armor name cost weight material . opt)
  (let ((appearance (or (and (= (length opt) 1) (car opt)) name)))
    (add-item name 'armor #f cost weight material appearance)))
(define (add-spe-armor name cost weight material)
  (add-item name 'armor #f cost weight material #f))

(define (armor-init)
  ; Dragon scales and DSM
  (for-each
   (lambda (color)
     (let ((ds (string-append color " dragon scales"))
	   (dsm (string-append color " dragon scale mail"))
	   (extra? (member color '("black" "gray" "silver"))))
       (add-reg-armor ds (if extra? 700 500) 40 'dragon-hide)
       (add-reg-armor dsm (if extra? 1200 900) 40 'dragon-hide)))
   '("red" "silver" "gray" "black" "white" "yellow" "green"
     "blue" "orange"))

  ; Plain armor
  (for-each
   (lambda (ls)
     (apply add-reg-armor ls))
   '(("T-shirt" 2 5 'cloth)
     ("Hawaiian shirt" 3 5 'cloth)

     ("leather jacket" 10 30 'leather)
     ("leather armor" 5 150 'leather)
     ("studded leather armor" 15 200 'leather)
     ("ring mail" 100 250 'iron)
     ("orcish ring mail" 80 250 'iron "crude ring mail")
     ("chain mail" 75 300 'iron)
     ("orcish chain mail" 75 300 'iron "crude chain mail")
     ("plate mail" 600 450 'iron)
     ("bronze plate mail" 400 450 'copper)
     ("crystal plate mail" 820 450 'glass)
     ("scale mail" 45 250 'iron)
     ("banded mail" 90 350 'iron)
     ("splint mail" 80 400 'iron)
     ("elven mithril coat" 240 150 'mithril)
     ("dwarvish mithril coat" 240 150 'mithril)

     ("orcish cloak" 40 10 'cloth "coarse mantelet")
     ("dwarvish cloak" 50 10 'cloth "hooded cloak")
     ("elven cloak" 60 10 'cloth "faded pall")
     ("robe" 50 15 'cloth)
     ("alchemy smock" 50 10 'cloth "apron")
     ("oilskin cloak" 50 10 'cloth "slippery cloak")
     ("leather cloak" 40 15 'leather "leather cloak")
     ("mummy wrapping" 2 3 'cloth)

     ("elven leather helm" 8 3 'leather "leather hat")
     ("dwarvish iron helm" 20 40 'iron "hard hat")
     ("orcish helm" 10 30 'iron "iron skull cap")
     ("dented pot" 8 10 'iron)
     ("fedora" 1 3 'cloth)

     ("iron shoes" 16 50 'iron "hard shoes")
     ("low boots" 8 10 'leather "walking shoes")
     ("high boots" 12 20 'leather "jackboots")

     ("large shield" 10 100 'iron)
     ("Uruk-hai shield" 7 50 'iron "white-handed shield")
     ("elven shield" 7 40 'wood "blue and green shield")
     ("small shield" 3 30 'wood "small shield")
     ("dwarvish roundshield" 10 100 'iron "large round shield")
     ("orcish shield" 7 50 'iron "red-eyed shield")
     ("shield of reflection" 50 50 'silver "polished silver shield")))

  ; Randomized armor
  (for-each
   (lambda (ls)
     (apply add-spe-armor ls))
   '(("cloak of magic resistance" 60 10 'cloth)
     ("cloak of displacement" 50 10 'cloth)
     ("cloak of protection" 50 10 'cloth)
     ("cloak of invisibility" 60 10 'cloth)

     ("helm of brilliance" 50 50 'iron)
     ("helm of opposite alignment" 50 50 'iron)
     ("helm of telepathy" 50 50 'iron)
     ("helmet" 10 30 'iron)

     ("dunce cap" 1 4 'cloth)
     ("cornuthaum" 80 4 'cloth)

     ("kicking boots" 8 15 'iron)
     ("elven boots" 8 15 'leather)
     ("levitation boots" 30 15 'leather)
     ("fumble boots" 30 20 'leather)
     ("jumping boots" 50 20 'leather)
     ("water walking boots" 50 20 'leather)
     ("speed boots" 50 20 'leather)

     ("leather gloves" 8 10 'leather)
     ("gauntlets of power" 50 30 'leather)
     ("gauntlets of dexterity" 50 10 'leather)
     ("gauntlets of fumbling" 50 10 'leather))))
