(define (amulets-init)
  (add-item "Amulet of Yendor" 'amulet #f 30000 20 #f #f)
  (add-item "amulet of magical breathing" 'amulet #f 150 20 #f #f)
  (add-item "cheap plastic imitation Amulet of Yendor" 'amulet #f 0 20 #f #f)
  (add-item "amulet of restful sleep" 'amulet #f 150 20 #f #f)
  (add-item "Eye of the Aethiopica" 'amulet #f 4000 20 #f #f)
  (add-item "amulet of reflection" 'amulet #f 150 20 #f #f)
  (add-item "amulet of strangulation" 'amulet #f 150 20 #f #f)
  (add-item "amulet of life saving" 'amulet #f 150 20 #f #f)
  (add-item "amulet versus poison" 'amulet #f 150 20 #f #f)
  (add-item "amulet of ESP" 'amulet #f 150 20 #f #f)
  (add-item "amulet of change" 'amulet #f 150 20 #f #f)
  (add-item "amulet of unchanging" 'amulet #f 150 20 #f #f))

(define amulet-appearances
  '("circular" "spherical" "oval" "triangular" "pyramidal"
    "square" "concave" "hexagonal" "octagonal"))

(define (unidentified-amulet? item)
  (let* ((name (item-name item))
	 (desc (string-drop-suffix " amulet" name)))
    (and desc (member desc amulet-appearances))))

(define (amulet? item)
  (let ((class (get-item-field item 1)))
    (or (and class (eq? class 'amulet))
	(string=? (item-name item) "amulet")
	(unidentified-amulet? item))))
