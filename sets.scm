#|
Set objects are represented in one of three ways:
1. The empty vector ~'#(set)~ represents ~{}~
2. The two-vector ~'#(set (x ...))~ represents ~{x, ...}~
3. The three-vector ~'#(set (x ...) T)~ represents ~{x, ...} ∪ T~
These forms are called "nil", "proper" and "improper" sets, respectively.

Note: this formulation ends up with redundant set representations.
A set object is normal form when it is written with the fewest possible
vector elements. That is to say, empty sets are nil, sets with a known
size are proper, and all other sets are improper (the tail must be
some non-set object which represents a "potential" set).
|#

(define (nil-set? x)
  (and (vector? x)
       (= 1 (vector-length x))
       (eq? (vector-ref x 0) 'set)))

(define (proper-set? x)
  (and (vector? x)
       (= (vector-length x) 2)
       (eq? (vector-ref x 0) 'set)))

(define (improper-set? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) 'set)))

(define (set? x)
  (and (vector? x)
       (>= 3 (vector-length x) 1)
       (eq? (vector-ref x 0) 'set)))

(define ∅         '#(set))
(define empty-set ∅)

(define nonnil-set? (disjoin proper-set? improper-set?))

(define (set-tail st)
  (cond
   [(improper-set? st) (set-tail (vector-ref st 2))]
   [(set? st)          ∅]
   [else               st]))

(define (set-head st)
  (cond
   [(proper-set? st)   (vector-ref st 1)]
   [(improper-set? st) (append (vector-ref st 1) (set-head (vector-ref st 2)))]
   [else               '()]))

(define (set-parts st)
  (values (set-head st) (set-tail st)))

(define (make-set head tail)
  (define-values (h t) (set-parts `#(set ,head ,tail)))
  (cond
   [(null? h)    t]
   [(nil-set? t) `#(set ,h)]
   [else         `#(set ,h ,t)]))

(define (set-pair? obj)
  (and (set? obj) (pair? (set-head obj))))

(define (set-null? obj)
  (and (set? obj) (null? (set-head obj))))

(define (set-first set)
  (car (set-head set)))

(define (set-rest set)
  (make-set
   (cdr (set-head set))
   (set-tail set)))

(define (set-cons elem set)
  (make-set (list elem) set))

(define (set->list set)
  (cond
   [(nonempty-set? set) (append (set-head set) (set->list (set-tail set)))]
   [(null-set? set)     '()]
   [else                set]))

(define (list->set lst)
  (make-set lst ∅))

(define (set . xs)
  (list->set xs))

(define (set* x . xs)
  (if (null? xs)
      x
      (set-cons x (apply set* xs))))
