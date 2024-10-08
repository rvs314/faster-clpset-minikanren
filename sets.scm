(define-record-type null-set
  (protocol
   (lambda (new)
     (let ([canonical (new)])
       (lambda ()
         canonical)))))

(define set-null? null-set?)

(define ∅         (make-null-set))
(define empty-set (make-null-set))

(record-type-equal-procedure
 (record-type-descriptor null-set)
 eq?)

(record-type-hash-procedure
 (record-type-descriptor null-set)
 (lambda _ 0))

(record-writer
 (record-type-descriptor null-set)
 (lambda (obj prt wrt)
   (display-string "∅" prt)))

(define-record-type nonempty-set
  (fields head tail)
  (protocol
   (lambda (new)
     (lambda (head tail)
       (if (null? head)
           tail
           (new head tail))))))

(define (set-head st)
  (if (nonempty-set? st)
      (nonempty-set-head st)
      '()))

(define (set-tail st)
  (if (nonempty-set? st)
      (nonempty-set-tail st)
      st))

(define (set-parts st)
  (values (set-head st) (set-tail st)))

(define set-pair? nonempty-set?)

(define (set-first set)
  (car (nonempty-set-head set)))

(define (set-rest set)
  (make-nonempty-set
   (cdr (nonempty-set-head set))
   (nonempty-set-tail set)))

(define (set-cons elem set)
  (if (nonempty-set? set)
      (make-nonempty-set
       (cons elem (nonempty-set-head set))
       (nonempty-set-tail set))
      (make-nonempty-set
       (list elem)
       set)))

(define (set? obj)
  (or (null-set? obj) (set-pair? obj)))

(record-type-equal-procedure
 (record-type-descriptor nonempty-set)
 (lambda (x y =)
   (and (= (set-head x) (set-head y))
        (= (set-tail x) (set-tail y)))))

(record-writer
 (record-type-descriptor nonempty-set)
 (lambda (obj prt wrt)
   (display "{" prt)
   (let loop ([head (nonempty-set-head obj)]
              [tail (nonempty-set-tail obj)])
     (cond
      [(and (pair? head) (null? (cdr head)))
       (wrt (car head) prt)
       (unless (set-null? tail)
         (display " | " prt)
         (wrt tail prt))
       (display "}" prt)]
      [else
       (wrt (car head) prt)
       (display " " prt)
       (loop (cdr head) tail)]))))

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
