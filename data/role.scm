(define (role-alignments role)
  (case role
    ((archeologist caveman valkyrie) '(lawful neutral))
    ((barbarian ranger wizard) '(neutral chaotic))
    ((healer tourist) '(neutral))
    ((knight samurai) '(lawful))
    ((priest monk) '(lawful neutral chaotic))
    ((rogue) '(chaotic))
    (else #f)))

(define (role-races role)
  (let ((x
	 (case role
	   ((archeologist caveman) '(dwarf gnome))
	   ((barbarian rogue) '(orc))
	   ((healer) '(gnome))
	   ((knight monk samurai tourist) '())
	   ((priest) '(elf))
	   ((ranger wiz) '(elf gnome orc))
	   ((valkyrie) '(dwarf)))))
    (and x (cons 'human x))))

(define (god-alignment god)
  (cond ((member god '("Quetzalcoatl" "Mitra" "Anu" "Athena" "Lugh"
		       "Shan Lai Ching" "Issek" "Mercury" "Amaterasu Omikami"
		       "Blind Io" "Tyr" "Ptah"))
	 'lawful)
	((member god '("Camaxtli" "Crom" "Ishtar" "Hermes" "Brigit"
		       "Chih Sung-tzu" "Mog" "Venus" "Raijin"
		       "The Lady" "Odin" "Thoth"))
	 'neutral)
	((member god '("Huhetotl" "Set" "Anshar" "Poseidon" "Manannan Mac Lir"
		       "Huan Ti" "Kos" "Mars" "Susanowo"
		       "Offler" "Loki" "Anhur"))
	 'chaotic)
	(else #f)))

