;; The empty set is the vector '#(set)
(define (set? x)
  (and (vector? x)
       (eq? (vector-ref x 0) 'set)))

(define (null-set? x)
  (and (set? x)
       (= 1 (vector-length x))))

(define ∅         '#(set))
(define empty-set ∅)

;; A nonempty-set is the vector '#(set H T)
(define (nonempty-set? x)
  (and (set? x)
       (eq? (vector-length x) 3)))

(define (nonempty-set-head obj)
  (assert (nonempty-set? obj))
  (vector-ref obj 1))

(define (nonempty-set-tail obj)
  (assert (nonempty-set? obj))
  (vector-ref obj 2))

(define (make-nonempty-set head tail)
  (cond
   [(null? head) tail]
   [(nonempty-set? tail)
    `#(set ,(append head (set-head tail)) ,(set-tail tail))]
   [else
    `#(set ,head ,tail)]))

(define (set-head st)
  (if (nonempty-set? st)
      (nonempty-set-head st)
      '()))

(define (set-tail st)
  (if (nonempty-set? st)
      (set-tail (nonempty-set-tail st))
      st))

(define (set-parts st)
  (values (set-head st) (set-tail st)))

(define set-pair? nonempty-set?)

(define (set-first set)
  (car (set-head set)))

(define (set-rest set)
  (make-nonempty-set
   (cdr (set-head set))
   (set-tail set)))

(define (set-cons elem set)
  (make-nonempty-set (list elem) set))

(define (set->list set)
  (cond
   [(nonempty-set? set) (append (nonempty-set-head set)
                                (set->list (nonempty-set-tail set)))]
   [(null-set? set)     '()]
   [else                set]))

(define (list->set lst)
  (make-nonempty-set lst ∅))

(define (set . xs)
  (list->set xs))

(define (set* x . xs)
  (if (null? xs)
      x
      (set-cons x (apply set* xs))))
