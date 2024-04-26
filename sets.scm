(define-record-type set-null
  (protocol
   (lambda (new)
     (let ([canonical (new)])
       (lambda ()
         canonical)))))

(define ∅         (make-set-null))
(define empty-set (make-set-null))

(record-type-equal-procedure
 (record-type-descriptor set-null)
 eq?)

(record-type-hash-procedure
 (record-type-descriptor set-null)
 (lambda _ 0))

(record-writer
 (record-type-descriptor set-null)
 (lambda (obj prt wrt)
   (display-string "∅" prt)))

(define-record-type (set-pair set-cons set-pair?)
  (fields (immutable first set-first)
          (immutable rest set-rest)))

(define (set? obj)
  (or (set-null? obj) (set-pair? obj)))

(record-type-equal-procedure
 (record-type-descriptor set-pair)
 (lambda (x y =)
   (and (= (set-first x) (set-first y))
        (= (set-rest x) (set-rest y)))))

(record-type-hash-procedure
 (record-type-descriptor set-pair)
 (lambda (x rec-hash)
   (rec-hash (cons (set-first x) (set-rest x)))))

(record-writer
 (record-type-descriptor set-pair)
 (lambda (obj prt wrt)
   (display-string "{" prt)
   (wrt (set-first obj) prt)
   (let loop ([obj (set-rest obj)])
     (cond
      [(set-pair? obj)
       (display-string " " prt)
       (wrt (set-first obj) prt)
       (loop (set-rest obj))]
      [(set-null? obj)
       (display-string "}" prt)]
      [else
       (display-string " | " prt)
       (wrt obj prt)
       (display-string "}" prt)]))))

(define-values (set->list list->set)
  (let ([X->Y (lambda (X-first X-rest X-null? X-pair? Y-cons Y-null) 
                (define (loop obj)
                  (cond [(X-null? obj) Y-null]
                        [(X-pair? obj) (Y-cons (X-first obj)
                                               (loop (X-rest obj)))]
                        [else          obj]))
                loop)])
    (values
     (X->Y set-first set-rest set-null? set-pair? cons '())
     (X->Y car       cdr      null?     pair?     set-cons ∅))))

(define (set . xs)
  (list->set xs))

(define (set* x . xs)
  (if (null? xs)
      x
      (set-cons x (apply set* xs))))
