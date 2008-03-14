(define (rings-init)
  (for-each
   (lambda (ls)
     (let ((name (string-append "ring of " (car ls)))
	   (cost (cadr ls)))
       (add-item name 'ring #f cost 3 #f #f)))
   '(("fire resistance" 200) ("conflict" 300) ("poison resistance" 150)
     ("regeneration" 200) ("free action" 200) ("see invisible" 150)
     ("teleportation" 200) ("adornment" 100) ("searching" 200) ("stealth" 100)
     ("protection from shape changers" 100) ("slow digestion" 200)
     ("gain constitution" 150) ("gain strength" 150) ("increase accuracy" 150)
     ("polymorph" 300) ("sustain ability" 100) ("polymorph control" 300)
     ("protection" 100) ("invisibility" 150) ("teleport control" 300)
     ("shock resistance" 150) ("levitation" 200) ("aggravate monster" 150)
     ("cold resistance" 150) ("increase damage" 150) ("hunger" 100)
     ("warning" 100))))

(define ring-appearances
  '("wooden" "granite" "opal" "clay" "coral" "moonstone" "jade"
    "bronze" "agate" "topaz" "sapphire" "ruby" "diamond" "pearl"
    "iron" "brass" "copper" "twisted" "steel" "silver" "gold" "ivory"
    "emerald" "wire" "engagement" "shiny" "black onyx" "tiger eye"))

(define (unidentified-ring? item)
  (let* ((name (item-name item))
	 (desc (string-drop-suffix " ring" name)))
    (and desc (member desc ring-appearances))))

(define (ring? item)
  (let ((class (get-item-field item 1)))
    (or (and class (eq? class 'ring))
	(string=? (item-name item) "ring")
	(unidentified-ring? item))))