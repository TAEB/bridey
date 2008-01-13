(define-structure bridey-lib
  (export do-door)
  (open scheme
	
        ask
	state
	misc
	term
	botl
	
        srfi-1
	srfi-13
	byte-vectors
        big-util
        sorting
	tables)
  (files bridey-lib old-explore utilities commands messages))

;; (define-structure utilities
;;   (export monster-string?
;; 	  monster?
;; 	  item?
;; 	  fountain?
;; 	  throne?
;; 	  dung-feature?
;; 	  within-extents?
;; 	  dir->vi
;; 	  vi->dir
;; 	  eight-dirs
;; 	  neighbor-squares
;; 	  diagonal?
;; 	  weird-position?
;; 	  open-door?
;; 	  wall?
;; 	  boulder?
;; 	  bad-trap?
;; 	  dead-end?
;; 	  passable?
;; 	  seen?
;; 	  mark-seen
;; 	  door?
;; 	  mark-door
;; 	  unmark-door
;; 	  trap?
;; 	  mark-trap
;; 	  unmark-trap
;; 	  searched-for)
;;   (open scheme
;; 	srfi-1
;; 	byte-vectors
;; 	state
;; 	term
;; 	misc
;; 	commands
;; 	bridey-lib)
;;   (files utilities))

;; (define-structure commands
;;   (export far-look look get-inventory engraving objects-here move fight
;; 	  kick open-door close-door search pick-up get-msgs
;; 	  get-raw-output blind?

;; 	  hp str dex con int wis cha dlvl xplvl xp turn-count curhp maxhp

;; 	  ; item stuff
;; 	  quantity buc greased? errodeproof? errosion-level enchantment
;; 	  item-name item-group-name item-indiv-name unpaid? cost wielded?
;; 	  wielded-oh? quivered? alternate-weapon?)
;;   (open scheme
;; 	srfi-1
;; 	byte-vectors
;; 	misc
;; 	ask
;; 	state
;; 	term
;; 	utilities
;; 	bridey-lib)
;;   (files commands))

(define-structure ask
  (export ask-connect
	  ask
	  add-command)
  (open scheme
	sockets
	misc)
  (files ask))

(define-structure term
  (export term-init
	  term-process
	  ;term-draw

	  
	  get-row-plaintext


	  iterate-screen
	  get-coord
	  square-char
	  square-color
	  square-char-dir
	  square-color-dir)
  (open scheme
	srfi-1
	ascii
	byte-vectors
	misc

	ask)
  (files term))

(define-structure botl
  (export botl-update
	  str dex con int wis cha
	  align score dlvl gold
	  curhp maxhp curpw maxpw
	  ac xlvl xp turns)
  (open scheme
	srfi-13
	regexps
	misc
	term)
  (files botl))

(define-structure misc
  (export filter-map
	  call/cc
	  compose
	  specialize
	  first
	  assoc-replace
	  nchars-identical
	  bit-set?
	  set-bit
	  unset-bit
	  i->coord
	  coord->i
	  map-bv-ref
	  map-bv-set!
	  map-bv-modify!)
  (open scheme
	srfi-1
	big-util
	byte-vectors
	bitwise)
  (files misc))

(define-structure state
  (export set-state
	  get-state
	  has-state?
	  modify-state
	  cons-state

	  ;
	  *state*)
  (open scheme
	misc)
  (files state))
