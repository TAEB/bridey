(define (add-weapon name plural cost weight material appearance)
  (let ((e (vector name
		   'weapon
		   plural
		   cost
		   weight
		   material
		   (or appearance name))))
    (add-item-v name e)
    (if plural
	(add-item-v plural e))
    (if appearance
	(add-item-v appearance e))
    (if (member appearance
		'("stout spear" "runed arrow" "runed dagger" "crude dagger"
		  "runed spear" "crude arrow" "crude spear"
		  "throwing spear" "throwing star" "bamboo arrow"))
	(add-item-v (string-append appearance "s") e))))

(define (weapons-init)
  ; Dagger
  (add-weapon "dagger" "daggers" 4 10 'iron #f)
  (add-weapon "orcish dagger" "orcish daggers" 4 10 'iron "crude dagger")
  (add-weapon "elven dagger" "elven daggers" 4 10 'wood "runed dagger")
  (add-weapon "silver dagger" "silver daggers" 40 12 'silver #f)
  (add-weapon "athame" "athames" 4 10 'iron #f)
  ; Flail
  (add-weapon "flail" #f 4 15 'iron #f)
  ; Bow
  (add-weapon "bow" #f 60 30 'wood #f)
  (add-weapon "elven arrow" "elven arrows" 2 1 'wood "runed arrow")
  (add-weapon "orcish bow" #f 60 30 'wood "crude bow")
  (add-weapon "orcish arrow" "orcish arrows" 2 1 'iron "crude arrow")
  (add-weapon "yumi" #f 60 30 'wood "long bow")
  (add-weapon "ya" "ya" 4 1 'metal "bamboo arrow")
  (add-weapon "elven bow" #f 60 30 'wood "runed bow")
  (add-weapon "arrow" "arrows" 2 1 'iron #f)
  (add-weapon "silver arrow" "silver arrows" 5 1 'silver #f)
  ; Club
  (add-weapon "club" #f 3 30 'wood #f)
  (add-weapon "aklys" #f 4 15 'iron "thonged club")
  ; Two-handed sword
  (add-weapon "two-handed sword" #f 50 150 'iron #f)
  (add-weapon "tsurugi" #f 500 60 'metal "long samurai sword")
  ; Polearm
  (add-weapon "fauchard" #f 5 60 'iron "pole sickle")
  (add-weapon "voulge" #f 5 125 'iron "pole cleaver")
  (add-weapon "bill-guisarme" #f 7 120 'iron "hooked polearm")
  (add-weapon "bec-de-corbin" #f 8 100 'iron "beaked polearm")
  (add-weapon "partisan" #f 10 80 'iron "vulgar polearm")
  (add-weapon "glaive" #f 6 75 'iron "single-edged polearm")
  (add-weapon "guisarme" #f 5 80 'iron "pruning polearm")
  (add-weapon "ranseur" #f 6 50 'iron "hilted polearm")
  (add-weapon "halberd" #f 10 150 'iron "angled polearm")
  (add-weapon "bardiche" #f 7 120 'iron "long poleaxe")
  (add-weapon "spetum" #f 5 50 'iron "forked polearm")
  (add-weapon "lucern hammer" #f 7 150 'iron "pronged polearm")
  ; Whip
  (add-weapon "bullwhip" #f 4 20 'leather #f)
  (add-weapon "rubber hose" #f 3 20 'plastic #f)
  ; Spear
  (add-weapon "spear" "spears" 3 30 'iron #f)
  (add-weapon "orcish spear" "orcish spears" 3 30 'iron "crude spear")
  (add-weapon "silver spear" "silver spears" 40 36 'silver #f)
  (add-weapon "elven spear" "elven spears" 3 30 'wood "runed spear")
  (add-weapon "dwarvish spear" "dwarvish spears" 3 35 'iron "stout spear")
  ; Broadsword
  (add-weapon "broadsword" #f 10 70 'iron #f)
  (add-weapon "elven broadsword" #f 10 70 'wood "runed broadsword")
  (add-weapon "runesword" #f 300 40 'iron "runed broadsword")
  ; Knife
  (add-weapon "knife" #f 4 5 'iron #f)
  (add-weapon "crysknife" #f 100 20 'mineral #f)
  (add-weapon "scalpel" #f 6 5 'metal #f)
  (add-weapon "worm tooth" #f 2 20 'none #f)
  (add-weapon "stiletto" #f 4 5 'iron #f)
  ; Long sword
  (add-weapon "long sword" #f 15 40 'iron #f)
  (add-weapon "katana" #f 80 40 'iron "samurai sword")
  ; Short sword
  (add-weapon "short sword" #f 10 30 'iron #f)
  (add-weapon "orcish short sword" #f 10 30 'iron "crude short sword")
  (add-weapon "dwarvish short sword" #f 10 30 'iron "broad short sword")
  (add-weapon "elven short sword" #f 10 30 'wood "runed short sword")
  ; Quarterstaff
  (add-weapon "quarterstaff" #f 5 40 'wood "staff")
  ; Pick-axe
  (add-weapon "dwarvish mattock" #f 50 120 'iron "broad pick")
  ; Axe
  (add-weapon "axe" #f 8 60 'iron #f)
  (add-weapon "battle-axe" #F 40 120 'iron "double-headed axe")
  ; Crossbow
  (add-weapon "crossbow" #f 40 50 'wood #f)
  (add-weapon "crossbow bolt" "crossbow bolts" 2 1 'iron #f)
  ; Lance
  (add-weapon "lance" #f 10 180 'iron #f)
  ; Mace
  (add-weapon "mace" #f 5 30 'iron #f)
  ; Unicorn horn
  ; (in tools.scm)
  ; Bommerang
  (add-weapon "boomerang" "boomerangs" 20 5 'wood #f)
  ; Trident
  (add-weapon "trident" #f 5 25 'iron #f)
  ; Darts
  (add-weapon "dart" "darts" 2 1 'iron #f)
  ; Morning star
  (add-weapon "morning star" #f 10 120 'iron #f)
  ; Shuriken
  (add-weapon "shuriken" "shuriken" 5 1 'iron "throwing star")
  ; Sling
  (add-weapon "sling" #f 20 3 'leather #f)
  ; Scimitar
  (add-weapon "scimitar" #f 15 40 'iron "curved sword")
  ; Hammer
  (add-weapon "war hammer" #f 5 50 'iron #f)
  ; Saber
  (add-weapon "silver saber" #f 75 40 'silver "silver saber")
  ; Javelin
  (add-weapon "javelin" "javelins" 3 20 'iron "throwing spear"))
