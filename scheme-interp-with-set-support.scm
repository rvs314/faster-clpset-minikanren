;; Initial Loads
(begin
  ;; Chez-specific code to get better debug info.
  (eval-when (eval load)
    (optimize-level 0)
    (debug-level 3))

  (load "mk-vicare.scm")
  (load "sets.scm")
  (load "mk.scm")
  (load "test-check.scm"))

;; William E. Byrd
;; Saturday May 24, 2025
;; Updated Friday June 20, 2025

;; Scheme interpreter with built-in support for sets and set
;; operations, implemented with CLP(Set)

;; ? Do I need to explictly tag set values ?
;; ? Do I need to worry about quoted sets ?
;; ? How best to implement `set?` and other type predicates ?

;; TODO:
;; * add additional set operations

(define (evalo expr val)
  (expr-evalo expr `((empty-set . ,(set))) val))

(define (expr-evalo expr env val)
  (conde
    ((numbero expr) (== expr val))
    ((fresh (datum)
       (== `(quote ,datum) expr)
       (== datum val)
       (absento 'clos datum)
       (not-in-envo 'quote env)))
    ((fresh (x e)
       (symbolo x)
       (== `(lambda (,x) ,e) expr)
       (== `(clos ,x ,e ,env) val)
       (not-in-envo 'lambda env)))
    ((symbolo expr) (lookupo expr env val))
    ((fresh (e1 e2 elem s)
       (== `(set-cons ,e1 ,e2) expr)
       (== (set-cons elem s) val)
       (expr-evalo e1 env elem)
       (expr-evalo e2 env s)
       (not-in-envo 'set-cons env)))
    ((fresh (e1 e2 v1 v2)
       (== `(cons ,e1 ,e2) expr)
       (== `(,v1 . ,v2) val)
       (expr-evalo e1 env v1)
       (expr-evalo e2 env v2)
       (not-in-envo 'cons env)))
    ((fresh (e1 e2 v1 v2)
       (== `(equal? ,e1 ,e2) expr)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val)))
       (expr-evalo e1 env v1)
       (expr-evalo e2 env v2)
       (not-in-envo 'equal? env)))
    ((fresh (e1 e2 x e env^ arg)
       (== `(,e1 ,e2) expr)
       (symbolo x)
       (expr-evalo e1 env `(clos ,x ,e ,env^))
       (expr-evalo e2 env arg)
       (expr-evalo e `((,x . ,arg) . ,env^) val)))))

(define (not-in-envo x env)
  (fresh ()
    (symbolo x)
    (conde
      ((== '() env))
      ((fresh (y v env^)
         (== `((,y . ,v) . ,env^) env)
         (symbolo y)
         (=/= x y)
         (not-in-envo x env^))))))

(define (lookupo x env val)
  (fresh (y v env^)
    (symbolo x)
    (symbolo y)
    (== `((,y . ,v) . ,env^) env)
    (conde
      ((== x y) (== v val))
      ((=/= x y) (lookupo x env^ val)))))

(test "sets-equal?-5ee"
  (run* (q)
    (expr-evalo `(equal? (set-cons 3 (set-cons 4 empty-set))
                         (set-cons 4 (set-cons 3 empty-set)))
                `((empty-set . ,(set)))
                q))
  '(#t))

(test "sets-equal?-5e"
  (run* (q)
    (evalo '(equal? (set-cons 3 (set-cons 4 empty-set))
                    (set-cons 4 (set-cons 3 empty-set)))
           q))
  '(#t))

(test "sets-equal?-5f-1"
  (run-unique* (q)
    (evalo '(equal? (set-cons 3 (set-cons 4 empty-set))
                    (set-cons 4 (set-cons 5 empty-set)))
           q))
  '(#f))

(test "sets-equal?-5f-2"
  (run* (q)
    (evalo '(equal? (set-cons 3 (set-cons 4 empty-set))
                    (set-cons 4 (set-cons 5 empty-set)))
           q))
  '(#f #f #f #f #f #f #f #f #f))

(test "cons-application-quote-1"
  (run 1 (q) (evalo '((lambda (z) (cons (quote z) z)) (quote cat)) q))
  '((z . cat)))

(test "empty-set-1"
  (run* (q) (evalo 'empty-set q))
  (list (set)))

(test "quine-1"
  (run 2 (q) (evalo q q))
  '((_.0 (num _.0))
    (((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 clos)) ((_.0 cons)) ((_.0 quote)))
     (sym _.0))))

(test "evalo-free-10k"
  (length (run 10000 (e v) (evalo e v)))
  10000)

(test "set-cons-1"
  (run* (q)
    (evalo '(set-cons (quote cat)
                      (set-cons (quote dog)
                                (set-cons (quote cat)
                                          empty-set)))
           q))
  (list (set 'cat 'dog)))

(test "sets-equal?-1"
  (run* (q)
    (evalo '(equal? empty-set
                    empty-set)
           q))
  '(#t))

(test "sets-equal?-2"
  (run* (q)
    (evalo '(equal? (set-cons (quote cat) empty-set)
                    empty-set)
           q))
  '(#f))

(test "sets-equal?-3"
  (run* (q)
    (evalo '(equal? (set-cons (quote cat) empty-set)
                    (set-cons (quote cat) empty-set))
           q))
  '(#t))

(test "sets-equal?-4"
  (run* (q)
    (evalo '(equal? (set-cons (quote dog) (set-cons (quote cat) empty-set))
                    (set-cons (quote dog) (set-cons (quote cat) empty-set)))
           q))
  '(#t))

(test "sets-equal?-5"
  (run* (q)
    (evalo '(equal? (set-cons (quote cat) (set-cons (quote dog) empty-set))
                    (set-cons (quote dog) (set-cons (quote cat) empty-set)))
           q))
  '(#t))

(test "sets-equal?-5b"
  (run* (q)
    (evalo '((lambda (v1)
               ((lambda (v2)
                  (cons (equal? v1 v2) (equal? v1 v2)))
                (set-cons (quote dog) (set-cons (quote cat) empty-set))))
             (set-cons (quote cat) (set-cons (quote dog) empty-set)))
           q))
  '((#t . #t)))

(test "sets-equal?-5c"
  (run* (q)
    (evalo '((lambda (v1)
               ((lambda (v2)
                  (cons (equal? v1 v2) (equal? v1 v2)))
                (set-cons 3 (set-cons 4 empty-set))))
             (set-cons 4 (set-cons 3 empty-set)))
           q))
  '((#t . #t)))

(test "sets-equal?-5d"
  (run* (q)
    (evalo '((lambda (v1)
               ((lambda (v2)
                  (equal? v1 v2))
                (set-cons 3 (set-cons 4 empty-set))))
             (set-cons 4 (set-cons 3 empty-set)))
           q))
  '(#t))

(test "sets-equal?-5e"
  (run* (q)
    (evalo '(equal? (set-cons 3 (set-cons 4 empty-set))
                    (set-cons 4 (set-cons 3 empty-set)))
           q))
  '(#t))

(test "without-interp-1"
  (let ((empty-set (set)))
    (run* (q)
      (== (set-cons 3 (set-cons 4 empty-set))
          (set-cons 4 (set-cons 3 empty-set)))))
  '(_.0))

(test "without-interp-2"
  (let ((empty-set (set)))
    (run* (q)
      (=/= (set-cons 3 (set-cons 4 empty-set))
           (set-cons 4 (set-cons 3 empty-set)))))
  '())


(test "b"
  (run* (e2)
    (evalo `((lambda (v1)
               (cons (equal? v1 (set-cons 5 (set-cons 6 v1)))
                     (equal? v1 (set-cons 5 (set-cons 6 v1)))))
             (quote ,e2))
           '(#t . #f)))
  '())

(test "c"
  (run* (e v)
    (evalo `(set-cons 5 (set-cons 6 (quote ,e)))
           v))
  '(((_.0 #(set (5 6) _.0))
     (set _.0) (absento (clos _.0)))))

(test "d"
  (run* (e v)
    (evalo `(set-cons 5 (quote ,e))
           v))
  '(((_.0 #(set (5) _.0))
     (set _.0) (absento (clos _.0)))))

(test "e"
  (run* (e v)
    (evalo `((lambda (v1) v1)
             (set-cons 5 (set-cons 6 (quote ,e))))
           v))
  '(((_.0 #(set (5 6) _.0))
     (set _.0) (absento (clos _.0)))))

(test "f"
  (run-unique* (e v)
    (evalo `((lambda (v1)
               (equal? (set-cons 5 (set-cons 6 v1))
                       (set-cons 6 (set-cons 5 v1))))
             (quote ,e))
           v))
  '(((_.0 #t)
     (set _.0) (absento (clos _.0)))
    ((#(set (5) _.0) #t)
     (set _.0) (absento (clos _.0)))
    ((#(set (5 6) _.0) #t)
     (set _.0) (absento (clos _.0)))
    ((#(set (6) _.0) #t)
     (set _.0) (absento (clos _.0)))))

(test "g"
  (run* (s1 s2)
    (fresh (x)
      (seto x)
      (absento 'clos x)
      (== (set-cons 5 x) s1)
      (== (set-cons 5 (set-cons 6 x)) s2)
      (=/= s1 s2)))
  '(((#(set (5) _.0) #(set (5 6) _.0))
     (set _.0)
     (absento (clos _.0))
     (∉ (6 _.0)))))

(test "h"
  (run-unique* (s1 s2)
    (fresh (x)
      (seto x)
      (absento 'clos x)
      (== (set-cons 5 x) s1)
      (== (set-cons 5 (set-cons 6 x)) s2)
      (== s1 s2)))
  '(((#(set (5 6) _.0) #(set (5 6) _.0))
     (set _.0)
     (absento (clos _.0)))))

(test "i"
  (run* (e)
    (evalo `(equal? (set-cons 6 (quote ,e)) (quote ,e))
           '#t))
  '((#(set (6) _.0)
     (set _.0) (absento (clos _.0)))))

(test "simple-equal?-test"
  (run* (q)
    (evalo `((lambda (v1)
               ((lambda (v2)
                  (cons (equal? v1 v2) (equal? v1 v2)))
                5))
             5)
           q))
  '((#t . #t)))

(test "sets-equal?-6"
  (run* (q)
    (evalo '(equal? (set-cons (quote cat) (set-cons (quote cat) empty-set))
                    (set-cons (quote cat) empty-set))
           q))
  '(#t))

(test "sets-equal?-7"
  (run* (q)
    (evalo '(equal? (set-cons (quote cat) empty-set)
                    (set-cons (quote cat) (set-cons (quote cat) empty-set)))
           q))
  '(#t))

(test "==-sets-1"
  (run* (q)
    (== (set-cons (quote cat) (set-cons (quote dog) empty-set))
        (set-cons (quote dog) (set-cons (quote cat) empty-set))))
  '(_.0))

(test "=/=-sets-1"
  (run* (q)
    (=/= (set-cons (quote cat) (set-cons (quote dog) empty-set))
         (set-cons (quote dog) (set-cons (quote cat) empty-set))))
  '())

(test "sets-cons-5"
  (run* (q)
    (evalo '(cons (set-cons (quote cat) (set-cons (quote dog) empty-set))
                  (set-cons (quote dog) (set-cons (quote cat) empty-set)))
           q))
  (list (cons (set 'cat 'dog) (set 'cat 'dog))))
