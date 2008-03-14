(define (potions-init)
  (for-each
   (lambda (ls)
     (let ((name (string-append "potion of " (car ls)))
	   (plural (string-append "potions of " (car ls)))
	   (cost (cadr ls)))
       (add-item name 'potion plural cost 20 #f #f)))
   '(("invisibility" 150) ("polymorph" 200) ("full healing" 200)
     ("levitation" 200) ("monster detection" 150) ("healing" 100) ("oil" 250)
     ("fruit juice" 50) ("sleeping" 100) ("hallucination" 100) ("speed" 200)
     ("booze" 50) ("see invisible" 50) ("gain ability" 300) ("gain level" 300)
     ("acid" 250) ("object detection" 150) ("paralysis" 300)
     ("enlightenment" 200) ("water" #f) ("extra healing" 100)
     ("confusion" 100) ("sickness" 50) ("restore ability" 100)
     ("blindness" 150) ("gain energy" 150)))
  
  ; pseudo-items
  (for-each
   (lambda (str)
     (add-item-v str (vector "potion of water" 'potion "potions of water"
			     100 20 #f #f)))
   '("potion of holy water" "potions of holy water"
     "potion of unholy water" "potions of unholy water")))

(define potion-appearances
  '("ruby" "pink" "orange" "yellow" "emerald" "cyan" "magenta"
    "purple-red" "puce" "milky" "swirly" "bubbly" "smoky"
    "cloudy" "effervescent" "black" "golden" "brown" "fizzy"
    "dark" "white" "murky" "dark green" "sky blue" "brilliant blue"))

(define (potion? item)
  (let ((class (get-item-field item 1)))
    (or (and class (eq? class 'potion))
	(let ((name (item-name item)))
	  (or (member name '("potion" "potions"))
	      (let ((desc (or (string-drop-suffix " potion" name)
			      (string-drop-suffix " potions" name))))
		(and desc (member desc potion-appearances))))))))