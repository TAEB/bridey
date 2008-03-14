(define (spellbooks-init)
  (add-item "Book of the Dead" 'spellbook #f 10000 20 #f "papyrus spellbook")
  (add-item "spellbook of blank paper" 'spellbook #f 0 50 #f "plain spellbook")
  (for-each
   (lambda (ls)
     (let ((name (string-append "spellbook of " (car ls)))
	   (cost (cadr ls)))
       (add-item name 'spellbook #f cost 50 #f #f)))
   '(("fireball" 400) ("detect monsters" 100) ("magic missile" 200)
     ("detect treasure" 400) ("teleport away" 600) ("levitation" 400)
     ("jumping" 100) ("finger of death" 700) ("polymorph" 600)
     ("cure sickness" 300) ("extra healing" 300) ("detect unseen" 300)
     ("cause fear" 300) ("dig" 500) ("identify" 300) ("restore ability" 400)
     ("invisibility" 400) ("wizard lock" 200) ("create monster" 200)
     ("slow monster" 200) ("clairvoyance" 300) ("drain life" 200)
     ("charm monster" 300) ("blank paper" 0) ("healing" 100)
     ("create familiar" 600) ("cone of cold" 400) ("detect food" 200)
     ("magic mapping" 500) ("turn undead" 600) ("haste self" 300)
     ("light" 100) ("force bolt" 100) ("sleep" 100) ("protection" 100)
     ("cure blindness" 200) ("confuse monster" 200) ("knock" 100)
     ("remove curse" 300) ("stone to flesh" 300) ("cancellation" 700))))

(define spellbook-appearances
  '("parchment" "vellum" "ragged" "mottled" "stained"
    "cloth" "leather" "white" "pink" "red" "orange" "yellow"
    "velvet" "turquoise" "cyan" "indigo" "magenta" "purple"
    "violet" "tan" "plaid" "gray" "wrinkled" "dusty" "bronze"
    "copper" "silver" "gold" "glittering" "shining" "dull"
    "thin" "thick" "dog eared" "light green" "dark green"
    "light blue" "dark blue" "light brown" "dark blue"))

(define (unidentified-spellbook? item)
  (let* ((name (item-name item))
	 (desc (string-drop-suffix " spellbook" name)))
    (and desc (member desc spellbook-appearances))))

(define (spellbook? item)
  (let ((class (get-item-field item 1)))
    (or (and class (eq? class 'spellbook))
	(string=? (item-name item) "spellbook")
	(unidentified-spellbook? item))))
