(define-structure main
  (export)
  (open scheme
	tasks bridey-lib nethack item monster term utilities misc)
  (files main))

(define-structure tasks
  (export add-task delete-task task-initial)
  (open scheme srfi-1
	bridey-lib state utilities misc)
  (files tasks))

(define-structure bridey-lib
  (export process-turn move fight search wait go-towards go-to
	  push-action push-action-go wall-curved? walk
	  initial-state)
  (open scheme srfi-1 srfi-2 srfi-13 byte-vectors sorting
	nethack item monster messages pathfinding state misc term botl
	utilities scraper parse)
  (files bridey-lib commands walls))

(define-structure messages
  (export process-message)
  (open scheme srfi-1 srfi-13 tables
	item parse state misc term botl utilities)
  (files messages))

(define-structure monster
  (export monster-init monster-valid?
	  monster-symbol monster-base-level monster-speed monster-ac monster-mr
	  monster-alignment monster-leaves-corpse? monster-weight
	  monster-nutrition monster-size monster-resistances
	  monster-resistances-conveyed monster-color monster-breathless?
	  monster-vegan? monster-vegetarian?
	  monster-always-hostile? monster-always-peaceful?
	  monster-peace-minded?)
  (open scheme tables srfi-2
	state)
  (files monster (data monsters)))

(define-structure item
  (export item-init item-name item-identity item-appearance item-slot
	  item-quantity item-class
	  artifact? scroll? tool? ring? potion? spellbook? armor? food?
	  artifact-base-item artifact-alignment quest-artifact?
	  artifact-role
	  item-figurine-of item-figurine? item-statue-of item-statue?
	  item-tin-of item-egg-of item-corpse-of item-tin? item-egg?
	  item-historic-statue? item-corpse? item-price
	  item-recharges item-charges item-enchantment item-diluted?
	  item-greased? item-poisoned? item-lit? item-we-laid? item-chained-to?
	  item-quivered? item-alt-weapon? item-partly-eaten? item-partly-used?
	  item-fooproof? item-called item-named item-max-erosion item-buc
	  item-cost item-weight item-material item-wielded?
	  item-wielded-offhand? item-wielded-either?
	  item-worn? item-worn-left? item-worn-right?
	  item-add-to-stack item-adjust-quantity
	  item-stackable? item-nutrition item-set-priest item-set-samurai)
  (open scheme tables regexps srfi-1 srfi-13
	monster misc)
  (files item (data item-amulets) (data item-armor) (data item-food)
	 (data item-gems) (data item-potions) (data item-rings)
	 (data item-scrolls) (data item-spellbooks) (data item-tools)
	 (data item-wands) (data item-weapons) (data item-artifacts)
	 (data item-japanese)))

(define-structure pathfinding
  (export find-path find-path-to find-path-towards find-path-hard)
  (open scheme srfi-1 sorting
	console scraper utilities term misc)
  (files pathfinding))

(define-structure soko
  (export)
  (open scheme srfi-1 srfi-13
	bridey-lib pathfinding state term utilities misc)
  (files soko))

(define-structure utilities
  (export valid-coord? monster? item? fountain? throne? engulfed?
	  dung-feature? trap-type square-clear? within-extents? dir->vi vi->dir
	  neighbor-squares diagonal? weird-position? open-door? boulder? bad-trap?
	  dead-end? passable? wall? searched-for orthogonal? adjacent?
	  min-distance
	  square-info-init square-info-load-level
	  visited? mark-visited seen? mark-seen
	  door? unmark-door open-door? mark-open-door closed-door?
	  mark-closed-door locked-door? mark-locked-door
	  trap? mark-trap unmark-trap
	  embedded? mark-embedded unmark-embedded same-level?
	  square-covered-by-item? set-square-covered-by
	  square-covered-match-current? unmark-square-covered-by-item
	  mark-all-corridors-seen
	  maybe-add-corpse add-fountain remove-fountain
	  decrement-item create-level
	  send-event)
  (open scheme srfi-1 srfi-13 byte-vectors
	monster item term state botl misc scraper parse)
  (files utilities))
	
(define-structure term
  (export term-init term-process term-got-partial?
	  get-row-plaintext term-match-string?
	  iterate-screen get-coord
	  square-char square-color square-inverse? square-glyph
	  square-char-dir square-color-dir square-inverse-dir? square-glyph-dir
	  term-find-symbol)
  (open scheme srfi-1 ascii byte-vectors bitwise
	console misc)
  (files term))

(define-structure pty
  (export pty-init pty-end pty-read-expect pty-send-expect)
  (open scheme sockets srfi-1 srfi-13
	term misc)
  (files (interfaces pty)))

(define-structure telnet
  (export telnet-init telnet-end telnet-read-expect telnet-send-expect)
  (open scheme sockets byte-vectors
	term misc parse)
  (files (interfaces telnet)))

(define-structure nethack
  (export nethack-init nethack-end send-expect read-expect
	  expect-generic expect-menu expect-more expect-no-change expect-dunno)
  (open scheme srfi-13 c-system-function
	pty telnet botl term parse misc)
  (files nethack expect))

(define-structure parse
  (export match-before-cur? at-question? at-menu? at-last-page? read-messages
	  at-more? inventory-item? split-inventory-item chop-punct)
  (open scheme srfi-1 srfi-2 srfi-13 regexps
	misc term)
  (files parse))

(define-structure scraper
  (export get-inventory get-discoveries far-look redraw-screen get-objects-here
	  do-look read-topl)
  (open scheme srfi-1 srfi-2 srfi-13
	state nethack botl misc term parse)
  (files scraper select-coord))

(define-structure botl
  (export botl-update botl-visible?
	  str dex con int wis cha
	  align score dlvl gold
	  curhp maxhp curpw maxpw
	  ac xlvl xp turns)
  (open scheme srfi-1 srfi-13 regexps
	misc term)
  (files botl))

(define-structure misc
  (export call/cc compose specialize first assoc-replace assoc-delete
	  nchars-identical bit-set? set-bit unset-bit min-p
	  i->coord coord->i map-bv-ref map-bv-set! map-bv-modify!
	  char->number char->control char->control-string
	  range reverse-range
	  string-drop-prefix string-drop-suffix
	  char-vowel?
	  identity delete-first)
  (open scheme srfi-1 srfi-13 bitwise byte-vectors)
  (files misc))

(define-structure state
  (export set-state get-state has-state? modify-state cons-state delete-state
	  *state*)
  (open scheme
	misc)
  (files state))

(define-structure wizmode
  (export wizmode-get-locations wizmode-location wizmode-levelport wizmode-map
	  wizmode-wish wizmode-create-monster wizmode-identify-inventory
	  wizmode-get-hunger wizmode-get-align)
  (open scheme srfi-13
	nethack utilities botl parse term misc)
  (files wizmode))

(define-structure console
  (export console-connect console-process-output console-quit)
  (open scheme srfi-1 sockets
	misc)
  (files (console console)))
