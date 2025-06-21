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
;; Saturday June 21, 2025
;;
;; The interpreter in `scheme-interp-with-set-support.scm` is very
;; limited, and doesn't support recursion, for example.  Let's add set
;; operations to the more sophisticated evaluator in full-interp, so
;; we can do things like write `occurs-free` and `occurs-bound` as
;; recursive Scheme functions (as opposed to miniKanren relations, as
;; in `occurs-free.scm`).
;;
;; The interpreter's initial environment includes `set-cons`,
;; `set-union`, `set-remove`, and the `empty-set`.
;;
;; * Beware the argument order to `set-remove`: as with `removeo`, the
;; set comes first, then the element to be removed!  Annoying...
;;
;; ? Do I need to explictly tag set values ?
;; ? Do I need to worry about quoted sets ?
;; ? How best to implement `set?` and other type predicates ?
;;
;; TODO:
;; * add additional set operations
;; * extend `match` to handle sets.  Not sure how much work that would be.

;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

(defrel (evalo expr val)
  (eval-expo expr initial-env val))

(defrel (eval-expo expr env val)
  (conde
    ((== `(quote ,val) expr)
     (absent-tago val)
     (not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ((symbolo expr) (lookupo expr env val))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(closure (lambda ,x ,body) ,env) val)
       (conde
         ;; Variadic
         ((symbolo x))
         ;; Multi-argument
         ((list-of-symbolso x)))
       (not-in-envo 'lambda env)))

    ((fresh (rator x rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; variadic
       (symbolo x)
       (== `((,x . (val . ,a*)) . ,env^) res)
       (eval-expo rator env `(closure (lambda ,x ,body) ,env^))
       (eval-expo body res val)
       (eval-listo rands env a*)))

    ((fresh (rator x* rands body env^ a* res)
       (== `(,rator . ,rands) expr)
       ;; Multi-argument
       (eval-expo rator env `(closure (lambda ,x* ,body) ,env^))
       (eval-listo rands env a*)
       (ext-env*o x* a* env^ res)
       (eval-expo body res val)))

    ((fresh (rator x* rands a* prim-id)
       (== `(,rator . ,rands) expr)
       (eval-expo rator env `(prim . ,prim-id))
       (eval-primo prim-id a* val)
       (eval-listo rands env a*)))

    ((handle-matcho expr env val))

    ((fresh (p-name x body letrec-body)
       ;; single-function variadic letrec version
       (== `(letrec ((,p-name (lambda ,x ,body)))
              ,letrec-body)
           expr)
       (conde
         ; Variadic
         ((symbolo x))
         ; Multiple argument
         ((list-of-symbolso x)))
       (not-in-envo 'letrec env)
       (eval-expo letrec-body
                  `((,p-name . (rec . (lambda ,x ,body))) . ,env)
                  val)))

    ((prim-expo expr env val))

    ))

(define empty-env '())

(defrel (lookupo x env t)
  (fresh (y b rest)
    (== `((,y . ,b) . ,rest) env)
    (conde
      ((== x y)
       (conde
         ((== `(val . ,t) b))
         ((fresh (lam-expr)
            (== `(rec . ,lam-expr) b)
            (== `(closure ,lam-expr ,env) t)))))
      ((=/= x y)
       (lookupo x rest t)))))

(defrel (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((,y . ,b) . ,rest) env)
       (=/= y x)
       (not-in-envo x rest)))))

(defrel (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expo a env v-a)
       (eval-listo d env v-d)))))

;; need to make sure lambdas are well formed.
;; grammar constraints would be useful here!!!
(defrel (list-of-symbolso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-symbolso d)))))

(defrel (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((,x . (val . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(defrel (eval-primo prim-id a* val)
  (conde
    [(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val))]
    ;; set related
    [(== prim-id 'set-cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== (set-cons a d) val))]
    [(== prim-id 'set-remove)
     (fresh (a d)
       (== `(,a ,d) a*)
       (removeo a d val))]
    [(== prim-id 'set-union)
     (fresh (a d)
       (== `(,a ,d) a*)
       (uniono a d val))]
    ;;
    [(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (not-tago val))]
    [(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (not-tago a))]
    [(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    [(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((symbolo v) (== #t val))
         ((numbero v) (== #f val))
         ((booleano v) (== #f val))
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((== '() v) (== #t val))
         ((=/= '() v) (== #f val))))]))

(defrel (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))
    ((and-primo expr env val))
    ((or-primo expr env val))
    ((if-primo expr env val))))

(defrel (boolean-primo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

(defrel (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

(defrel (ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (eval-expo e1 env v))
         ((=/= #f v)
          (eval-expo e1 env v)
          (ando `(,e2 . ,e-rest) env val)))))))

(defrel (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

(defrel (oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expo e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (eval-expo e1 env v))
         ((== #f v)
          (eval-expo e1 env v)
          (oro `(,e2 . ,e-rest) env val)))))))

(defrel (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expo e1 env t)
    (conde
      ((=/= #f t) (eval-expo e2 env val))
      ((== #f t) (eval-expo e3 env val)))))

(define initial-env `((empty-set . (val . ,(set))) ;; set related
                      (list . (val . (closure (lambda x x) ,empty-env)))
                      (not . (val . (prim . not)))
                      (equal? . (val . (prim . equal?)))
                      (symbol? . (val . (prim . symbol?)))
                      (cons . (val . (prim . cons)))
                      (set-cons . (val . (prim . set-cons))) ;; set related
                      (set-remove . (val . (prim . set-remove))) ;; set related
                      (set-union . (val . (prim . set-union))) ;; set related                      
                      (null? . (val . (prim . null?)))
                      (car . (val . (prim . car)))
                      (cdr . (val . (prim . cdr)))
                      . ,empty-env))

(defrel (not-tago val)
  (fresh ()
    (=/= 'closure val)
    (=/= 'prim val)))

(defrel (absent-tago val)
  (fresh ()
    (absento 'closure val)
    (absento 'prim val)))

(defrel (handle-matcho expr env val)
  (fresh (against-expr mval clause clauses)
    (== `(match ,against-expr ,clause . ,clauses) expr)
    (not-in-envo 'match env)
    (eval-expo against-expr env mval)
    (match-clauses mval `(,clause . ,clauses) env val)))

(defrel (not-symbolo t)
  (conde
    ((== '() t))
    ((booleano t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel (not-numbero t)
  (conde
    ((== '() t))
    ((booleano t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

(defrel (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

(defrel (literalo t)
  (conde
    ((== '() t))
    ((numbero t))
    ((symbolo t) (not-tago t))
    ((booleano t))))

(defrel (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

(defrel (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((,y . (val . ,v)) . ,rest) env1)
       (== `((,y . (val . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

(defrel (match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         (eval-expo result-expr env^ val)))
      ((p-no-match p mval '() penv)
       (match-clauses mval d env val)))))

(defrel (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (not-tago mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((,var . (val . ,mval)) . ,penv) penv-out)
       (not-in-envo var penv)))))

(defrel (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo var penv val)))

(defrel (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

(defrel (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

(defrel (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

(defrel (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (not-tago mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))


(test "occurs-free-list-version-1"
  (time
   (run* (q)
     (evalo `(letrec ((remove
                       (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (remove x (cdr l))))))))
               (letrec ((append
                         (lambda (l s)
                           (if (null? l)
                               s
                               (cons (car l) (append (cdr l) s))))))
                 (letrec ((occurs-free
                           (lambda (expr)
                             (match expr
                               ((? symbol? x)
                                (cons x '()))
                               (`(lambda (,x) ,e)
                                (remove x (occurs-free e)))
                               (`(cons ,e1 ,e2)
                                (append (occurs-free e1) (occurs-free e2)))))))
                   (occurs-free '(cons w (lambda (y) (cons y z)))))))
            q)))
  '((w z)))

(test "occurs-free-list-version-2"
  (time
   (run 5 (expr)
     (evalo `(letrec ((remove
                       (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (remove x (cdr l))))))))
               (letrec ((append
                         (lambda (l s)
                           (if (null? l)
                               s
                               (cons (car l) (append (cdr l) s))))))
                 (letrec ((occurs-free
                           (lambda (expr)
                             (match expr
                               ((? symbol? x)
                                (cons x '()))
                               (`(lambda (,x) ,e)
                                (remove x (occurs-free e)))
                               (`(cons ,e1 ,e2)
                                (append (occurs-free e1) (occurs-free e2)))))))
                   (occurs-free ',expr))))
            (list 'w 'z))))
  '((cons w z)
    ((lambda (_.0) (cons w z))
     (=/= ((_.0 w)) ((_.0 z)))
     (absento (closure _.0) (prim _.0)))
    ((cons w (lambda (_.0) z))
     (=/= ((_.0 z)))
     (absento (closure _.0) (prim _.0)))
    ((cons (lambda (_.0) w) z)
     (=/= ((_.0 w)))
     (absento (closure _.0) (prim _.0)))
    ((lambda (_.0) (lambda (_.1) (cons w z)))
     (=/= ((_.0 w)) ((_.0 z)) ((_.1 w)) ((_.1 z)))
     (absento
      (closure _.0)
      (closure _.1)
      (prim _.0)
      (prim _.1)))))
















(test "occurs-free-list-version-5a"
  ;; under-specification, we meet again!
  (time
   (run 1 (q)     
     (evalo `(letrec ((remove
                       (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (remove x (cdr l))))))))
               (letrec ((append
                         (lambda (l s)
                           (if (null? l)
                               s
                               (cons (car l) (append (cdr l) s))))))
                 (letrec ((occurs-free
                           (lambda (expr)
                             (match expr
                               ((? symbol? x)
                                (cons x '()))
                               (`(lambda (,x) ,e)
                                (,q x (occurs-free e)))
                               (`(cons ,e1 ,e2)
                                (append (occurs-free e1) (occurs-free e2)))))))
                   (occurs-free '(cons w (lambda (y) (cons y z)))))))
            (list 'w 'z))))
  '(((lambda _.0 '(z)) (=/= ((_.0 quote))) (sym _.0))))

(test "occurs-free-list-version-5b"
  (time
   (run 1 (q)     
     (evalo `(letrec ((remove
                       (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (remove x (cdr l))))))))
               (letrec ((append
                         (lambda (l s)
                           (if (null? l)
                               s
                               (cons (car l) (append (cdr l) s))))))
                 (letrec ((occurs-free
                           (lambda (expr)
                             (match expr
                               ((? symbol? x)
                                (cons x '()))
                               (`(lambda (,x) ,e)
                                (,q x (occurs-free e)))
                               (`(cons ,e1 ,e2)
                                (append (occurs-free e1) (occurs-free e2)))))))
                   (list (occurs-free '(lambda (c) (cons a c)))
                         (occurs-free '(cons w (lambda (y) (cons y z))))))))
            (list (list 'a) (list 'w 'z)))))
  '(remove))

#|
;; This version doesn't come back after a minute or two.
;; This example nicely shows the advange of set constraints
;; rather than using lists at the user level to represent sets.

(test "occurs-free-list-version-5c"
  ;; swap the elements of the list containing 'w and 'z
  (time
   (run 1 (q)     
     (evalo `(letrec ((remove
                       (lambda (x l)
                         (if (null? l)
                             '()
                             (if (equal? (car l) x)
                                 (cdr l)
                                 (cons (car l) (remove x (cdr l))))))))
               (letrec ((append
                         (lambda (l s)
                           (if (null? l)
                               s
                               (cons (car l) (append (cdr l) s))))))
                 (letrec ((occurs-free
                           (lambda (expr)
                             (match expr
                               ((? symbol? x)
                                (cons x '()))
                               (`(lambda (,x) ,e)
                                (,q x (occurs-free e)))
                               (`(cons ,e1 ,e2)
                                (append (occurs-free e1) (occurs-free e2)))))))
                   (list (occurs-free '(lambda (c) (cons a c)))
                         (occurs-free '(cons w (lambda (y) (cons y z))))))))
            (list (list 'a) (list 'z 'w)))))
  '(remove))
|#










;; Nice example of the expressive power of set constraints, compared
;; with the list-based version above.  For a truly fair comparison,
;; would need to compare against an evaliator in which the set
;; operations are built-in, in the form of list operations.
;;
;; In these simple experiments, at least, the list version seems
;; faster than the set version.  Staging should make the list version
;; faster still.  What is the advantage of the set version, other than
;; abstraction?  Abstraction in specifying the answer, I think.  An
;; advantage in abstraction for reasoning at the level of
;; specification.
;;
;; Ah!  Found a clear example.  Compare "occurs-free-list-version-5c"
;; versus "occurs-free-set-version-5c".  When synthesizing part of the
;; definition of occurs-free, reordering the elements in a returned
;; set doesn't have a noticable impact on the set version.  Reorder
;; the elements in a returned list, though, and the query doesn't come
;; back after minutes, when with the original ordering it succeeded in
;; 200 milliseconds.
(test "occurs-free-set-version-1"
  (time (run* (q)
          (evalo `(letrec ((occurs-free
                            (lambda (expr)
                              (match expr
                                ((? symbol? x)
                                 (set-cons x empty-set))
                                (`(lambda (,x) ,e)
                                 (set-remove (occurs-free e) x))
                                (`(cons ,e1 ,e2)
                                 (set-union (occurs-free e1) (occurs-free e2)))))))
                    (occurs-free '(cons w (lambda (y) (cons y z)))))
                 q)))
  '(#(set (w z))))

(test "occurs-free-set-version-2"
  (time (run 5 (expr)
          (evalo `(letrec ((occurs-free
                            (lambda (expr)
                              (match expr
                                ((? symbol? x)
                                 (set-cons x empty-set))
                                (`(lambda (,x) ,e)
                                 (set-remove (occurs-free e) x))
                                (`(cons ,e1 ,e2)
                                 (set-union (occurs-free e1) (occurs-free e2)))))))
                    (occurs-free ',expr))
                 (set 'w 'z))))
  '((cons w z)
    (cons z w)
    ((cons (cons w z) (lambda (_.0) _.0))
     (=/= ((_.0 closure)) ((_.0 prim)))
     (sym _.0))
    ((cons
       (cons w z)
       (cons (lambda (_.0) _.0) (lambda (_.1) _.1)))
     (=/= ((_.0 closure))
          ((_.0 prim))
          ((_.1 closure))
          ((_.1 prim)))
     (sym _.0 _.1))
    (cons w (cons z z))))

(test "occurs-free-set-version-3"
  (time (run 1 (q)
          (evalo `(letrec ((occurs-free
                            (lambda (expr)
                              (match expr
                                ((? symbol? x)
                                 (set-cons x ,q))
                                (`(lambda (,x) ,e)
                                 (set-remove (occurs-free e) x))
                                (`(cons ,e1 ,e2)
                                 (set-union (occurs-free e1) (occurs-free e2)))))))
                    (occurs-free '(cons w (lambda (y) (cons y z)))))
                 (set 'w 'z))))
  '(empty-set))

(test "occurs-free-set-version-4"
  (time (run 1 (q)
          (evalo `(letrec ((occurs-free
                            (lambda (expr)
                              (match expr
                                ((? symbol? x)
                                 (,q x empty-set))
                                (`(lambda (,x) ,e)
                                 (set-remove (occurs-free e) x))
                                (`(cons ,e1 ,e2)
                                 (set-union (occurs-free e1) (occurs-free e2)))))))
                    (occurs-free '(cons w (lambda (y) (cons y z)))))
                 (set 'w 'z))))
  '(set-cons))

(test "occurs-free-set-version-5a"
  ;; under-specification, we meet again!
  (time (run 1 (q)
          (evalo `(letrec ((occurs-free
                            (lambda (expr)
                              (match expr
                                ((? symbol? x)
                                 (set-cons x empty-set))
                                (`(lambda (,x) ,e)
                                 (,q (occurs-free e) x))
                                (`(cons ,e1 ,e2)
                                 (set-union (occurs-free e1) (occurs-free e2)))))))
                    (occurs-free '(cons w (lambda (y) (cons y z)))))
                 (set 'w 'z))))
  '(((lambda _.0 '#(set (z)))
     (=/= ((_.0 quote)))
     (sym _.0))))

(test "occurs-free-set-version-5b"
  (time (run 1 (q)
          (evalo `(letrec ((occurs-free
                            (lambda (expr)
                              (match expr
                                ((? symbol? x)
                                 (set-cons x empty-set))
                                (`(lambda (,x) ,e)
                                 (,q (occurs-free e) x))
                                (`(cons ,e1 ,e2)
                                 (set-union (occurs-free e1) (occurs-free e2)))))))
                    (list (occurs-free '(lambda (c) (cons a c)))
                          (occurs-free '(cons w (lambda (y) (cons y z))))))
                 (list (set 'a) (set 'w 'z)))))
  '(set-remove))

(test "occurs-free-set-version-5c"
  ;; swap the elements of the set containing 'w and 'z
  ;; no problem!
  (time (run 1 (q)
          (evalo `(letrec ((occurs-free
                            (lambda (expr)
                              (match expr
                                ((? symbol? x)
                                 (set-cons x empty-set))
                                (`(lambda (,x) ,e)
                                 (,q (occurs-free e) x))
                                (`(cons ,e1 ,e2)
                                 (set-union (occurs-free e1) (occurs-free e2)))))))
                    (list (occurs-free '(lambda (c) (cons a c)))
                          (occurs-free '(cons w (lambda (y) (cons y z))))))
                 (list (set 'a) (set 'z 'w)))))
  '(set-remove))

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

(test "evalo-free-400"
  (length (run 400 (e v) (evalo e v)))
  400)

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
     (set _.0) (absento (closure _.0) (prim _.0)))))

(test "d"
  (run* (e v)
    (evalo `(set-cons 5 (quote ,e))
           v))
  '(((_.0 #(set (5) _.0))
     (set _.0) (absento (closure _.0) (prim _.0)))))

(test "e"
  (run* (e v)
    (evalo `((lambda (v1) v1)
             (set-cons 5 (set-cons 6 (quote ,e))))
           v))
  '(((_.0 #(set (5 6) _.0))
     (set _.0) (absento (closure _.0) (prim _.0)))))

(test "f"
  (run-unique* (e v)
    (evalo `((lambda (v1)
               (equal? (set-cons 5 (set-cons 6 v1))
                       (set-cons 6 (set-cons 5 v1))))
             (quote ,e))
           v))
  '(((_.0 #t)
     (set _.0) (absento (closure _.0) (prim _.0)))
    ((#(set (5) _.0) #t)
     (set _.0) (absento (closure _.0) (prim _.0)))
    ((#(set (5 6) _.0) #t)
     (set _.0) (absento (closure _.0) (prim _.0)))
    ((#(set (6) _.0) #t)
     (set _.0) (absento (closure _.0) (prim _.0)))))

(test "g"
  (run* (s1 s2)
    (fresh (x)
      (seto x)
      (absento 'closure x)
      (== (set-cons 5 x) s1)
      (== (set-cons 5 (set-cons 6 x)) s2)
      (=/= s1 s2)))
  '(((#(set (5) _.0) #(set (5 6) _.0))
     (set _.0)
     (absento (closure _.0))
     (∉ (6 _.0)))))

(test "h"
  (run-unique* (s1 s2)
    (fresh (x)
      (seto x)
      (absento 'closure x)
      (== (set-cons 5 x) s1)
      (== (set-cons 5 (set-cons 6 x)) s2)
      (== s1 s2)))
  '(((#(set (5 6) _.0) #(set (5 6) _.0))
     (set _.0)
     (absento (closure _.0)))))

(test "i"
  (run* (e)
    (evalo `(equal? (set-cons 6 (quote ,e)) (quote ,e))
           '#t))
  '((#(set (6) _.0)
     (set _.0)
     (absento (closure _.0) (prim _.0)))))

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
