(define (wands-init)
  (for-each
   (lambda (ls)
     (let ((name (string-append "wand of " (car ls)))
	   (cost (cadr ls)))
       (add-item name 'wand #f cost 7 #f #f)))
   '(("create monster" 200) ("wishing" 500) ("make invisible" 150)
     ("death" 500) ("slow monster" 150) ("locking" 150) ("undead turning" 150)
     ("secret door detection" 150) ("light" 100) ("fire" 175) ("probing" 150)
     ("polymorph" 200) ("cancellation" 200) ("opening" 150) ("cold" 175)
     ("teleportation" 200) ("enlightenment" 150) ("lightning" 175)
     ("magic missile" 150) ("striking" 150) ("sleep" 175) ("digging" 150)
     ("speed monster" 150) ("nothing" 100))))

(define wand-appearances
  '("glass" "balsa" "crystal" "maple" "pine" "oak" "ebony" "marble"
    "tin" "brass" "copper" "silver" "platinum" "iridium" "zinc"
    "aluminum" "uranium" "iron" "steel" "hexagonal" "short"
    "runed" "long" "curved" "forked" "spiked" "jeweled"))

(define (wand? item)
  (let ((class (get-item-field item 1)))
    (or (and class (eq? class 'wand))
	(let ((name (item-name item)))
	  (or (string=? name "wand")
	      (let ((desc (string-drop-suffix " wand" name)))
		(and desc (member desc wand-appearances))))))))