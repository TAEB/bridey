; debug
(define *state* #f)

(define (set-state state . ls)
  (let ((s (apply internal-set-state state ls)))
    (set! *state* s)
    s))

(define (internal-set-state state . ls)
  (if (or (null? ls) (null? (cdr ls)))
      state
      (let ((name (car ls))
	    (value (cadr ls)))
	(apply internal-set-state
	       (assoc-replace (list name value) state)
	       (cddr ls)))))

(define (modify-state state . ls)
  (if (or (null? ls) (null? (cdr ls)))
      state
      (let ((name (car ls))
	    (f (cadr ls)))
	(modify-state (set-state state name (f (get-state state name)))
		      (cddr ls)))))

(define (cons-state state . ls)
  (if (or (null? ls) (null? (cdr ls)))
      state
      (let ((name (car ls))
	    (value (cadr ls)))
	(cons-state (modify-state
		     state
		     name (lambda (ls)
			    (cons value (or ls '()))))
		    (cddr ls)))))
      

(define (get-state state name)
  (let ((w (assq name state)))
    (if #f;(not w)
	(begin (display "NO STATE: ")
	       (write name)
	       (newline)))
    (and w (cadr w))))

(define (has-state? state name) (assoc name state))

(define (delete-state state name)
  (let ((new (assoc-delete name state)))
    (set! *state* new)
    new))