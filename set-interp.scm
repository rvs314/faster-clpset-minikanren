(eval-when (compile load eval)
  (load "./test-check.scm")
  (load "./sets.scm")
  (load "./mk-vicare.scm")
  (load "./mk.scm"))

(define _.0 '_.0)
(define _.1 '_.1)
(define _.2 '_.2)
(define _.3 '_.3)
(define _.4 '_.4)
(define _.5 '_.5)


(define empty-env ∅)
(define (ext-envo x v env env^)
  (fresh ()
    (symbolo x)
    ;; In this encoding we add to the `env` both `x` and the pair
    ;; binding `x` to `v`.  This allows us to use `(!ino x env)` to
    ;; ensure `x` is not already bound in `env`.
    (conde
      ((!ino x env)
       ;; Case 1: not shadowing `x`
       ;;
       ;; `x` isn't in `env`, so we are free to add the binding
       ;; between `x` and `v`, along with `x` so we can check
       ;; for shadowing in the future.
       (== env^ (set* x `(,x . ,v) env)))
      ((fresh (v^ env-minus-x-binding)
         ;; Case 2: shadowing `x`
         ;;
         ;; There is already a binding between `x` and `v^` in `env`,
         ;; so we must create a new environment `env-minus-x-binding`
         ;; that is identical to `env`, minus the `x`/`v^` binding.
         ;; We can safely extend `env-minus-x-binding` with the
         ;; `x`/`v` binding to produce `env^`.
         (ino x env)
         (removeo env `(,x . ,v^) env-minus-x-binding)
         (uniono (set `(,x . ,v^)) env-minus-x-binding env)
         (== env^ (set* x `(,x . ,v) env-minus-x-binding)))))))

(define (lookupo x env val)
  (fresh ()
    (symbolo x)
    (ino x env)
    (ino `(,x . ,val) env)))
(define (not-in-envo x env)
  (fresh ()
    (symbolo x)
    (!ino x env)))

(define (evalo expr val)
  (eval-expro expr empty-env val))

(define (eval-expro expr env val)
  (conde
    ;; We don't have `absento` in this version of mk, so punt by
    ;; breaking `quote` into cases, using `=/=`, and avoiding `car`,
    ;; `cdr`, or other destructors.
    ((== '(quote ()) expr)
     (== '() val)
     (not-in-envo 'quote env))
    ((== `(quote ,val) expr)
     (symbolo val)
     (=/= 'closure val)
     (not-in-envo 'quote env))
    ((fresh (a d)
       (== `(quote (,a . ,d)) expr)
       (== `(,a . ,d) val)
       (=/= 'closure a)
       (=/= 'closure d)
       (not-in-envo 'quote env)))
    ((symbolo expr)
     (lookupo expr env val))
    ((fresh (x e)
       (== `(lambda (,x) ,e) expr)
       (== `(closure ,x ,e ,env) val)
       (symbolo x)))
    ((fresh (e*)
       (== `(list . ,e*) expr)
       (not-in-envo 'list env)
       (eval-listo e* env val)))
    ((fresh (e1 e2 x e env^ arg env^^)
       (== `(,e1 ,e2) expr)
       (eval-expro e1 env `(closure ,x ,e ,env^))
       (eval-expro e2 env arg)
       (ext-envo x arg env^ env^^)
       (eval-expro e env^^ val)))))

(define (eval-listo e* env v*)
  (conde
    ((== '() e*) (== '() v*))
    ((fresh (e e-rest v v-rest)
       (== `(,e . ,e-rest) e*)
       (== `(,v . ,v-rest) v*)
       (eval-expro e env v)
       (eval-listo e-rest env v-rest)))))

(test "ext-envo-1"
      (run-unique* (q)
        (fresh (env env^)
          (== (list env env^) q)
          (ext-envo 'z 4 empty-env env)
          (ext-envo 'w 3 env env^)))
      `((,(set 'z '(z . 4)) ,(set 'w 'z '(w . 3) '(z . 4)))))

(test "ext-envo-2"
  (run-unique* (q)
    (fresh (env env^)
      (== (list env env^) q)
      (ext-envo 'z 4 empty-env env)
      (ext-envo 'z 3 env env^)))
  `((,(set 'z '(z . 4)) ,(set 'z '(z . 3)))))

(test "evalo-1"
  (run-unique* (q)
    (evalo '((lambda (y) y) (list 'cat 'dog)) q))
  '((cat dog)))


(test "evalo-2"
  (run-unique* (q)
    (evalo '((lambda (y) (((lambda (z) z) (lambda (w) w)) y)) (list 'cat 'dog)) q))
  '((cat dog)))

(test "evalo-3"
  (run-unique* (q)
    (evalo '((lambda (y) (((lambda (y) y) (lambda (y) y)) y)) (list 'cat 'dog)) q))
  '((cat dog)))

(test "evalo-4"
  (run-unique* (q)
    (evalo '(((lambda (list) (list list)) (lambda (list) list)) (list 'cat 'dog)) q))
  '((cat dog)))

(test "evalo-5"
  (run-unique* (q)
    (evalo '(((lambda (x) (lambda (y) (list x y))) 'cat) 'dog) q))
  '((cat dog)))

(test "evalo-6"
  (run-unique* (q)
    (evalo '(((lambda (x) (lambda (x) (list x x))) 'cat) 'dog) q))
  '((dog dog)))

(test "eval-expro-1"
  (run-unique* (env)
    (eval-expro 'z env 'cat))
  `((,(set* 'z '(z . cat) '_.0) (set _.0))))

(test "eval-expro-2a"
  (run-unique* (env)
    (!ino 'quote env)
    (eval-expro '(quote cat) env 'cat))
  '((_.0 (set _.0) (∉ (quote _.0)))))

(test "eval-expro-2ab"
  (run-unique 2 (env)
    (ino 'quote env)
    (eval-expro '(quote cat) env 'cat))
  `((,(set* 
           'cat
           'quote
           '(cat . _.2)
           '(quote . (closure _.0 (quote cat) _.1))
           '_.3)
     (=/= ((_.0 quote)))
     (sym _.0)
     (set _.1 _.3)
     (∉ (_.0 _.1) (quote _.1)))
    (,(set*
           'cat
           'quote
           '(cat . cat)
           '(quote . (closure _.0 _.0 _.1))
           '_.2)
     (sym _.0)
     (set _.1 _.2)
     (∉ (_.0 _.1)))))

(test "eval-expro-2b"
  (run-unique 3 (env)
    (!ino 'lambda env)
    (!ino 'quote env)
    (eval-expro `((lambda (y) y) 'cat) env 'cat))
  `((_.0
     (set _.0)
     (∉ (lambda _.0) (quote _.0) (y _.0)))
    (,(set* 'y '(y . _.0) _.1)
     (set _.1)
     (∉ (lambda _.1) (quote _.1) (y _.1) ((y . _.0) _.1)))
    (,(set* '(y . cat) _.0)
     (set _.0)
     (∉ (lambda _.0) (quote _.0) (y _.0)))))

(test "eval-expro-2c"
  (run-unique 3 (q)
    (fresh (e env)
      (== (list e env) q)
      (symbolo e)
      (!ino 'lambda env)
      (!ino 'quote env)
      (eval-expro `((lambda (y) ,e) 'cat) env 'cat)))
  `(((_.0 ,(set* _.0 '(_.0 . cat) _.1))
     (=/= ((_.0 lambda)) ((_.0 quote)) ((_.0 y)))
     (sym _.0)
     (set _.1)
     (∉ (lambda _.1) (quote _.1) (y _.1)))
    ((y _.0)
     (set _.0)
     (∉ (lambda _.0) (quote _.0) (y _.0)))
    ((y ,(set* '(y . cat) _.0))
     (set _.0)
     (∉ (lambda _.0) (quote _.0) (y _.0)))))


(test "eval-expro-2c"
  (run-unique 3 (q)
    (fresh (e env)
      (== (list e env) q)
      (symbolo e)
      (!ino 'lambda env)
      (!ino 'quote env)
      (eval-expro `((lambda (y) ,e) 'cat) env 'cat)))
  `(((_.0 ,(set* '_.0 '(_.0 . cat) '_.1))
     (=/= ((_.0 lambda)) ((_.0 quote)) ((_.0 y)))
     (sym _.0)
     (set _.1)
     (∉ (lambda _.1) (quote _.1) (y _.1)))
    ((y _.0)
     (set _.0)
     (∉ (lambda _.0) (quote _.0) (y _.0)))
    ((y ,(set* '(y . cat) '_.0))
     (set _.0)
     (∉ (lambda _.0) (quote _.0) (y _.0)))))


(test "eval-expro-3"
  (run-unique 3 (q)
    (fresh (e env)
      (== (list e env) q)
      (symbolo e)
      (!ino 'lambda env)
      (!ino 'list env)
      (!ino 'quote env)
      (eval-expro `((lambda (y) ,e) (list 'cat 'dog)) env '(cat dog))))
  `(((_.0 ,(set* _.0 '(_.0 cat dog) _.1)) (=/= ((_.0 lambda)) ((_.0 list)) ((_.0 quote)) ((_.0 y))) (sym _.0) (set _.1) (∉ (lambda _.1) (list _.1) (quote _.1) (y _.1)))
    ((y _.0) (set _.0) (∉ (lambda _.0) (list _.0) (quote _.0) (y _.0)))
    ((y ,(set* '(y cat dog) _.0)) (set _.0) (∉ (lambda _.0) (list _.0) (quote _.0) (y _.0)))))


(test "quine-1"
      (run-unique 1 (q)
       (evalo q q))
      '((((lambda (_.0) (list _.0 (list (quote quote) _.0))) (quote (lambda (_.0) (list _.0 (list (quote quote) _.0))))) (=/= ((_.0 list)) ((_.0 quote))) (sym _.0))))

(test "twine-1"
  (run-unique 1 (ans)
    (fresh (p q)
      (== (list p q) ans)
      (=/= p q)
      (evalo p q)
      (evalo q p)))
  '((((quote ((lambda (_.0) (list (quote quote) (list _.0 (list (quote quote) _.0)))) (quote (lambda (_.0) (list (quote quote) (list _.0 (list (quote quote) _.0)))))))
      ((lambda (_.0) (list (quote quote) (list _.0 (list (quote quote) _.0)))) (quote (lambda (_.0) (list (quote quote) (list _.0 (list (quote quote) _.0)))))))
     (=/= ((_.0 list)) ((_.0 quote))) (sym _.0))))
