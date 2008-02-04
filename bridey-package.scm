(define-structure bridey-lib
  (export process-turn move fight search wait go-towards go-to)
  (open scheme srfi-1 srfi-13 big-util byte-vectors sorting
	nethack messages pathfinding state misc term botl utilities
	scraper parse)
  (files bridey-lib commands))

(define-structure messages
  (export process-message)
  (open scheme srfi-1 tables
	parse state misc term botl utilities)
  (files messages))

(define-structure pathfinding
  (export find-path find-path-to find-path-towards)
  (open scheme srfi-1 sorting
	console scraper utilities term misc)
  (files pathfinding))

(define-structure soko
  (export)
  (open scheme srfi-1 srfi-13
	bridey-lib pathfinding state term utilities misc)
  (files soko))

(define-structure utilities
  (export valid-coord? monster-string? monster? item? fountain? throne? engulfed?
	  dung-feature? trap-type square-clear? within-extents? dir->vi vi->dir
	  neighbor-squares diagonal? weird-position? open-door? boulder? bad-trap?
	  dead-end? passable? wall? searched-for orthogonal? adjacent? min-distance
	  square-info-init square-info-load-level
	  visited? mark-visited seen? mark-seen
	  door? unmark-door open-door? mark-open-door closed-door?
	  mark-closed-door locked-door? mark-locked-door
	  trap? mark-trap unmark-trap
	  embedded? mark-embedded unmark-embedded same-level?
	  mark-all-corridors-seen)
  (open scheme srfi-1 srfi-13 byte-vectors
	term state botl	misc scraper)
  (files utilities))
	
(define-structure term
  (export term-init term-process
	  get-row-plaintext term-match-string?
	  iterate-screen get-coord
	  square-char square-color square-inverse?
	  square-char-dir square-color-dir square-inverse-dir?
	  term-find-symbol)
  (open scheme srfi-1 ascii byte-vectors bitwise
	console misc)
  (files term))

(define-structure pty
  (export pty-init pty-read-expect pty-send-expect)
  (open scheme sockets srfi-1 srfi-13
	term misc)
  (files pty))

(define-structure telnet
  (export telnet-init telnet-read-expect telnet-send-expect)
  (open scheme sockets byte-vectors
	term misc parse)
  (files telnet))

(define-structure nethack
  (export nethack-init send-expect read-expect
	  expect-generic expect-menu expect-more expect-no-change expect-dunno)
  (open scheme srfi-13 c-system-function
	pty telnet botl term parse misc)
  (files nethack expect))

(define-structure parse
  (export match-before-cur? at-question? at-menu? at-last-page? read-messages
	  at-more? inventory-item? split-inventory-item)
  (open scheme big-util srfi-1 srfi-2 srfi-13 regexps
	misc term)
  (files parse))

(define-structure scraper
  (export get-inventory far-look redraw-screen)
  (open scheme big-util srfi-1 srfi-2 srfi-13
	nethack misc term parse)
  (files scraper select-coord))

(define-structure botl
  (export botl-update
	  str dex con int wis cha
	  align score dlvl gold
	  curhp maxhp curpw maxpw
	  ac xlvl xp turns)
  (open scheme srfi-13 regexps
	misc term)
  (files botl))

(define-structure misc
  (export filter-map call/cc compose specialize first assoc-replace assoc-delete
	  nchars-identical bit-set? set-bit unset-bit min-p
	  i->coord coord->i map-bv-ref map-bv-set! map-bv-modify!
	  char->number char->control char->control-string)
  (open scheme srfi-1 big-util bitwise byte-vectors)
  (files misc))

(define-structure state
  (export set-state get-state has-state? modify-state cons-state delete-state
	  *state* gs ss)
  (open scheme
	misc)
  (files state))

(define-structure wizmode
  (export wizmode-get-locations wizmode-location wizmode-levelport wizmode-map
	  wizmode-wish wizmode-create-monster wizmode-identify-inventory)
  (open scheme srfi-13
	nethack utilities botl parse term misc)
  (files wizmode))

(define-structure console
  (export console-connect console-process-output console-quit)
  (open scheme srfi-1 sockets
	misc)
  (files console))