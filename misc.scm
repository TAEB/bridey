; scheme48's srfi-1 has this function wrong... can't accept multiple lists
;(define (filter-map f ls . lss)
;  (filter identity (apply map (cons f (cons ls lss)))))

; s48 doesn't define this
(define call/cc call-with-current-continuation)

(define (compose f . fs)
  (if (null? fs)
      f
      (lambda x (f (apply (apply compose fs) x)))))

;; ; incompatible with SRFI-1, but what use is (define first car)?
;; (define (first p? ls)
;;   (and (not (null? ls))
;;        (if (p? (car ls))
;; 	   (car ls)
;; 	   (first p? (cdr ls)))))

(define (assoc-replace entry alist)
  (cond ((null? alist) (list entry))
	((equal? (car entry) (caar alist))
	 (cons entry (cdr alist)))
	(else
	 (cons (car alist) (assoc-replace entry (cdr alist))))))

(define (assoc-delete key alist)
  (cond ((null? alist) '())
	((equal? key (caar alist))
	 (cdr alist))
	(else
	 (cons (car alist) (assoc-delete key (cdr alist))))))


; currying.
(define (specialize f . ls)
  (lambda more
    (apply f (append ls more))))

(define (nchars-identical s1 s2)
  (let ((len (min (string-length s1) (string-length s2))))
    (let loop ((i 0))
      (if (= i len)
	  0
	  (if (char=? (string-ref s1 i) (string-ref s2 i))
	      (+ 1 (loop (+ i 1)))
	      (loop (+ i 1)))))))

(define (bit-set? n bit)
  (not (zero? (bitwise-and n (arithmetic-shift 1 bit)))))

(define (set-bit n bit)
  (bitwise-ior n (arithmetic-shift 1 bit)))

(define (unset-bit n bit)
  (bitwise-and n (bitwise-not (arithmetic-shift 1 bit))))

(define (min-p <? head . rest)
  (cond
   ((null? rest) head)
   ((<? head (car rest)) (apply min-p <? head (cdr rest)))
   (else (apply min-p <? rest))))

(define (coord->i c)
  (let ((x (- (car c) 1))
	(y (- (cadr c) 1)))
    (+ (* y 80) x)))

(define (i->coord i)
  (list (+ 1 (modulo i 80))
	(+ 1 (quotient i 80))))

(define (map-bv-ref vec coord)
  (byte-vector-ref vec (coord->i coord)))

(define (map-bv-set! vec coord v)
  (byte-vector-set! vec (coord->i coord) v))

(define (map-bv-modify! vec coord f)
  (let ((i (coord->i coord)))
    (byte-vector-set! vec i (f (byte-vector-ref vec i)))))

(define (char->number c)
  (- (char->integer c) 48))

(define (char->control char)
  (integer->char (- (char->integer (char-downcase char)) 96)))

(define (char->control-string char)
  (string (char->control char)))

(define (range start finish)
  (iota (+ (- finish start) 1) start 1))

(define (reverse-range start finish)
  (iota (+ (- start finish) 1) start -1))

(define (string-drop-prefix pre str)
  (and (string-prefix? pre str)
       (string-drop str (string-length pre))))

(define (string-drop-suffix suf str)
  (and (string-suffix? suf str)
       (string-drop-right str (string-length suf))))

; nethack uses American English
(define (char-vowel? c)
  (case (char-downcase c)
    ((#\a #\e #\i #\o #\u) #t)
    (else #f)))

(define (identity x) x)

(define (delete-first p ls)
  (cond ((null? ls) '())
	((p (car ls)) (cdr ls))
	(cons (car ls)
	      (delete-first (cdr ls)))))
