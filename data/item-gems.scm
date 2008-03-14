(define (gems-init)
  (for-each
   (lambda (color)
     (let ((name (string-append "worthless piece of " color "glass"))
	   (plural (string-append "worthless pieces of " color "glass")))
       (add-item name 'gem plural 0 1 #f #f)))
   gem-colors)

  (for-each
   (lambda (x)
     (let* ((name (car x))
	    (plural (if (string=? name "ruby")
			"rubies"
			(string-append name "s")))
	    (cost (cadr x)))
       (add-item name 'gem plural cost 1 #f #f)))
   '(("dilithium crystal" 4500) ("diamond" 4000) ("ruby" 3500)
     ("jacinth stone" 3250) ("sapphire" 3000) ("black opal" 2500)
     ("emerald" 2500) ("turquoise stone" 2000) ("aquamarine stone" 1500)
     ("citrine stone" 1500) ("amber stone" 1000) ("topaz stone" 900)
     ("jet stone" 850) ("opal" 800) ("chrysoberyl stone" 700)
     ("garnet stone" 700) ("amethyst stone" 600) ("jasper stone" 500)
     ("fluorite stone" 400) ("jade stone" 300) ("agate stone" 200)
     ("obsidian stone" 200)))

  (add-item "loadstone" 'stone "loadstones" 1 500 #f #f)
  (add-item "luckstone" 'stone "luckstones" 60 10 #f #f)
  (add-item "flint stone" 'stone "flint stones" 1 10 #f #f)
  (add-item "rock" 'stone "rocks" 0 10 #f "rock"))

(define gem-colors
  '("black" "blue" "green" "orange" "red" "violet" "white" "yellow"
    "yellowish brown"))

(define (unidentified-gem? item)
  (let* ((name (item-name item))
	 (color (or (string-drop-suffix " gem" name)
		    (string-drop-suffix " gems" name))))
    (and color (member color gem-colors))))

