(test "free-lit/lit"
  (run* (p)
    (freeo 1 '())
    (freeo 1 '((2 . 3))))
  '(_.0))

(test "not-free-lit/lit"
  (run* (p)
    (freeo 1 '((1 . 3))))
  '())

(test "free-var/lit"
  (run* (p)
    (freeo p '((1 . 3))))
  '((_.0 (=/= ((_.0 1))))))

(test "free-lit/var"
  (run* (p)
    (fresh (j)
      (freeo 1 p)
      (== p (list j))))
  '((((_.0 . _.1)) (=/= ((_.0 1))))))

(test "free-var/var"
  (run* (p)
    (fresh (j)
      (freeo j p)
      (== p (list j))))
  '(((_.0 . _.1))))

(test "free-bound/lit"
  (run* (p)
    (== p '((2 . 3)))
    (freeo 9 p))
  '(((2 . 3))))

(test "not-free-bound/lit"
  (run* (p)
    (== p '((2 . 3)))
    (freeo 2 p))
  '())

(test "free-lit/bound"
  (run* (p)
    (== 9 p)
    (freeo p '((2 . 3))))
  '(9))

(test "not-free-lit/bound"
  (run* (p)
    (== 2 p)
    (freeo p '((2 . 3))))
  '())

(test "not-free-var/bound"
  (run* (p)
    (freeo 2 p)
    (symbolo p))
  '())

;; FIXME: This should be subsumed
(test "freeo with absento"
  (run* (p q)
    (freeo p q)
    (absento p q))
 '(((_.0 _.1)
    (lst _.1)
    (absento (_.0 _.1))
    (free (_.0 _.1)))))

;; Apparently, chez doesn't scope local calls to `load`,
;; as it's implemented in terms of `eval` against the global
;; interaction environment, so `load`ing the interpreter
;; shadows `lookupo`. Loading `mk.scm` seems to reinstate the
;; bindings for now. 
(begin
  (load "./simple-interp.scm")

  (test "current interpreter behvaior"
    (run 2 (env val)
      (eval-expro `(lambda (x) x) env val))
    '((() (closure x x ()))
      ((((_.0 . _.1)) (closure x x ((_.0 . _.1))))
       (=/= ((_.0 lambda))))))

  (set! not-in-envo freeo)

  (test "potential interpreter behvaior"
    (run 2 (env val)
      (eval-expro `(lambda (x) x) env val))
    '(((_.0 (closure x x _.0))
       (lst _.0)
       (free (lambda _.0)))))

  (load "./mk.scm"))

(printf "Lookup tests\n")

(test "ground lookup"
  (run* (_)
    (lookupo 1 '((1 . 2)) 2))
  '(_.0))

(test "ground lookup search"
  (run* (_)
    (lookupo 1 '((() . ()) (p . q) (a . b) (1 . 2)) 2))
  '(_.0))

(test "nonsense"
  (run* (_)
    (lookupo 1 999 2))
  '())

(test "failed ground search (not present)"
  (run* (_)
    (lookupo 1 '((2 . 3) (0 . 4) (a . b)) 2))
  '())

(test "failed ground search (different value)"
  (run* (_)
    (lookupo 1 '((2 . 3) (1 . 4) (a . b)) 2))
  '())

(test "lookup shadowing"
  (run* (_)
    (lookupo 1 '((1 . 2) (1 . 4) (a . b)) 2))
  '(_.0))

(test "lookup failing shadowing"
  (run* (_)
    (lookupo 1 '((1 . 4) (1 . 2) (a . b)) 2))
  '())

(test "lookup with fresh list"
  (run* (rst)
    (lookupo 1 rst 2))
  '((_.0 (lst _.0) (lookup (1 _.0 2)))))

(test "lookup with fresh key"
  (run* (x)
    (lookupo x '((3 . 2) (1 . 4) (a . 2)) 2))
  '(3 a))

(test "lookup with fresh value"
  (run* (x)
    (lookupo 3 '((3 . 2) (1 . 4) (a . 2)) x))
  '(2))

(test "most general query"
  (run* (p q r)
    (lookupo p r q))
  '(((_.0 _.1 _.2) (lst _.2) (lookup (_.0 _.2 _.1)))))

(test "partially ground list"
  (run* (r k1 k2 k3)
    (lookupo 1 `((,k1 . b) (,k2 . d) (,k3 . e) . ,r) 2))
  '(((_.0 _.1 _.2 _.3)
     (=/= ((_.1 1)) ((_.2 1)) ((_.3 1)))
     (lst _.0)
     (lookup (1 _.0 2)))))

(test "partially ground list (2)"
  (run* (r k1 k2 k3)
    (lookupo 1 `((,k1 . b) (,k2 . 2) (,k3 . e) . ,r) 2))
  '(((_.0 _.1 1 _.2) (=/= ((_.1 1))) (lst _.0))
    ((_.0 _.1 _.2 _.3)
     (=/= ((_.1 1)) ((_.2 1)) ((_.3 1)))
     (lst _.0)
     (lookup (1 _.0 2)))))

(test "paritally ground list (3)"
  (run* (r k1 k2 v3)
    (lookupo 1 `((,k1 . b) (,k2 . 2) (1 . ,v3) . ,r) 2))
  '(((_.0 _.1 1 _.2) (=/= ((_.1 1))) (lst _.0))
    ((_.0 _.1 _.2 2) (=/= ((_.1 1)) ((_.2 1))) (lst _.0))))

(test "multiple ground constraints"
  (run* (r)
    (lookupo 1 r 2)
    (lookupo 2 r 3))
  '((_.0 (lst _.0) (lookup (1 _.0 2) (2 _.0 3)))))

(test "conflicting ground constraints"
  (run* (r)
    (lookupo 1 r 2)
    (lookupo 1 r 3))
  '())

(test "partially ground constraints (1)"
  (run* (r p)
    (lookupo 1 r p)
    (lookupo 1 r 3))
  '(((_.0 3) (lst _.0) (lookup (1 _.0 3)))))

(test "partially ground constraints (2)"
  (run* (r p q)
    (lookupo 1 r p)
    (lookupo 1 r q))
  '(((_.0 _.1 _.1) (lst _.0) (lookup (1 _.0 _.1)))))

(test "partially ground constraints (3)"
  (run* (r p q s)
    (lookupo s r p)
    (lookupo 1 r q)
    (=/= p q))
  '(((_.0 _.1 _.2 _.3)
     (=/= ((_.1 _.2)) ((_.3 1)))
     (lst _.0)
     (lookup (1 _.0 _.2) (_.3 _.0 _.1)))))

(test "partially ground constraints (4)"
  (run* (a b c d e f g)
    (lookupo a `((,c . ,d) (,e . ,f) . ,g) b))
  '(((_.0 _.1 _.0 _.1 _.2 _.3 _.4)
     (lst _.4))
    ((_.0 _.1 _.2 _.3 _.0 _.1 _.4)
     (=/= ((_.0 _.2)))
     (lst _.4))
    ((_.0 _.1 _.2 _.3 _.4 _.5 _.6)
     (=/= ((_.0 _.2)) ((_.0 _.4)))
     (lst _.6)
     (lookup (_.0 _.6 _.1)))))

(test "lookupo vs free"
  (run* (a b c)
    (lookupo a c b)
    (freeo a c))
  '())

(test "lookupo vs free (2)"
  (run* (a b c)
    (lookupo a c b)
    (freeo 1 c))
  '(((_.0 _.1 _.2)
     (=/= ((_.0 1)))
     (lst _.2)
     (free (1 _.2))
     (lookup (_.0 _.2 _.1)))))

(test "lookupo vs free (2.5)"
  (run* (a b c)
    (freeo 1 c)
    (lookupo a c b))
  '(((_.0 _.1 _.2)
     (=/= ((_.0 1)))
     (lst _.2)
     (free (1 _.2))
     (lookup (_.0 _.2 _.1)))))

(test "lookupo vs free (3)"
  (run* (a b c)
    (lookupo a c b)
    (freeo b c)
    (== a b))
  '())

(test "many<->one"
  (run* (a b c)
    (lookupo 1 a 2)
    (lookupo 2 a 3)
    (lookupo 3 a 4)
    (freeo b a))
  '(((_.0 _.1 _.2)
     (=/= ((_.1 1)) ((_.1 2)) ((_.1 3)))
     (lst _.0)
     (free (_.1 _.0))
     (lookup (1 _.0 2) (2 _.0 3) (3 _.0 4)))))

(test "absento vs lookupo"
  (run* (a b)
    (absento a b)
    (lookupo a b 1))
  '())

(test "lookupo vs absento"
  (run* (a b)
    (absento a b)
    (lookupo a b 1))
  '())

(test "absento & lookupo"
  (run* (bad-el alist key val)
    (absento bad-el alist)
    (lookupo key alist val))
  '(((_.0 _.1 _.2 _.3)
     (lst _.1)
     (absento (_.0 _.1) (_.0 _.2) (_.0 _.3))
     (lookup (_.2 _.1 _.3)))))

(test "lookupo & absento"
  (run* (bad-el alist key val)
    (lookupo key alist val)
    (absento bad-el alist))
  '(((_.0 _.1 _.2 _.3)
     (lst _.1)
     (absento (_.0 _.1) (_.0 _.2) (_.0 _.3))
     (lookup (_.2 _.1 _.3)))))

(test "pidgeonhole-ground"
  (run* (a b c d t)
    (lookupo 1 t 2)
    (lookupo 3 t 4)
    (== t `((,a . ,b) (,c . ,d))))
  '((3 4 1 2 ((3 . 4) (1 . 2)))
    (1 2 3 4 ((1 . 2) (3 . 4)))))

(test "pidgeonhole-fresh"
  (run* (key1 key2 val1 val2 car1 cdr1 car2 cdr2 alist)
    (lookupo key1 alist val1)
    (lookupo key2 alist val2)
    (== alist `((,car1 . ,cdr1) (,car2 . ,cdr2))))
  
  '(;; The keys/values are the same, and are the first association
    (_.0 _.0 _.1 _.1 _.0 _.1 _.2 _.3 ((_.0 . _.1) (_.2 . _.3)))
    ;; The keys/values are different, with key2/val2 first 
    ((_.0 _.1 _.2 _.3 _.1 _.3 _.0 _.2 ((_.1 . _.3) (_.0 . _.2)))
     (=/= ((_.0 _.1))))
    ;; The keys/values are different, with key2/val2 first,
    ;; but both values are the same
    ((_.0 _.1 _.2 _.2 _.1 _.2 _.0 _.2 ((_.1 . _.2) (_.0 . _.2)))
     (=/= ((_.0 _.1))))
    ;; The keys/values are different, with key1/val1 first
    ((_.0 _.1 _.2 _.3 _.0 _.2 _.1 _.3 ((_.0 . _.2) (_.1 . _.3)))
     (=/= ((_.0 _.1))))
    ;; The keys/values are different, with key1/val1 first,
    ;; but both values are the same
    ((_.0 _.1 _.2 _.2 _.0 _.2 _.1 _.2 ((_.0 . _.2) (_.1 . _.2)))
     (=/= ((_.0 _.1))))
    ;; The keys/values are the same, and are the first association
    ((_.0 _.0 _.1 _.1 _.2 _.3 _.0 _.1 ((_.2 . _.3) (_.0 . _.1)))
     (=/= ((_.0 _.2))))))

(test "pidgeonhole-fails"
  (run* (α β a b c d w x y z t)
    (lookupo w t x)
    (lookupo y t z)
    (lookupo α t β)
    (=/= w y)
    (=/= w α)
    (=/= y α)
    (== t `((,a . ,b) (,c . ,d))))
  '())

(test "differently-typed-keys"
  (run* (k₁ k₂ alist)
    (lookupo k₁ alist 1)
    (lookupo k₂ alist 2)
    (symbolo k₁)
    (numbero k₂))
  ;; Disequality constraint is subsumed by types
  '(((_.0 _.1 _.2)
     (num _.1)
     (sym _.0)
     (lst _.2)
     (lookup (_.0 _.2 1) (_.1 _.2 2)))))

(test "same-typed-keys"
  (run* (k₁ k₂ alist)
    (lookupo k₁ alist 1)
    (lookupo k₂ alist 2)
    (symbolo k₁)
    (symbolo k₂))
  ;; Disequality constraint is explicit
  '(((_.0 _.1 _.2)
     (=/= ((_.0 _.1)))
     (sym _.0 _.1)
     (lst _.2)
     (lookup (_.0 _.2 1) (_.1 _.2 2)))))

(let ()
  (defrel (proper-listo exp env val)
    (conde
      ((== '() exp)
       (== '() val))
      ((fresh (a d t-a t-d)
         (== `(,a . ,d) exp)
         (== `(,t-a . ,t-d) val)
         (eval-expo a env t-a)
         (proper-listo d env t-d)))))

  (defrel (eval-expo exp env val)
    (freeo 'quote env)
    (freeo 'list env)
    (freeo 'lambda env)
    (conde
      ((fresh (v)
         (== `(quote ,v) exp)
         (absento 'closure v)
         (== v val)))
      ((fresh (a*)
         (listo a*)
         (== `(list . ,a*) exp)
         (proper-listo a* env val)))
      ((symbolo exp) (lookupo exp env val))
      ((fresh (rator rand x body env^ a)
         (== `(,rator ,rand) exp)
         (eval-expo rator env `(closure ,x ,body ,env^))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env^) val)))
      ((fresh (x body)
         (== `(lambda (,x) ,body) exp)
         (symbolo x)
         (== `(closure ,x ,body ,env) val)))))

  (test "5 quines"
    (run 5 (q) (eval-expo q '() q))
    '((((lambda (_.0) (list _.0 (list (quote quote) _.0)))
        '(lambda (_.0) (list _.0 (list (quote quote) _.0))))
       (=/= ((_.0 closure)) ((_.0 lambda)) ((_.0 list)) ((_.0 quote)))
       (sym _.0))
      (((lambda (_.0) (list _.0 (list ((lambda (_.1) 'quote) (quote _.2)) _.0)))
        '(lambda (_.0) (list _.0 (list ((lambda (_.1) 'quote) (quote _.2)) _.0))))
       (=/= ((_.0 closure))
            ((_.0 lambda))
            ((_.0 list))
            ((_.0 quote))
            ((_.1 closure))
            ((_.1 lambda))
            ((_.1 list))
            ((_.1 quote)))
       (sym _.0 _.1)
       (absento (closure _.2)))
      (((lambda (_.0) (list ((lambda (_.1) _.0) (quote _.2)) (list 'quote _.0)))
        '(lambda (_.0) (list ((lambda (_.1) _.0) (quote _.2)) (list 'quote _.0))))
       (=/= ((_.0 _.1))
            ((_.0 closure))
            ((_.0 lambda))
            ((_.0 list))
            ((_.0 quote))
            ((_.1 closure))
            ((_.1 lambda))
            ((_.1 list))
            ((_.1 quote)))
       (sym _.0 _.1)
       (absento (closure _.2)))
      (((lambda (_.0) (list _.0 ((lambda (_.1) (list 'quote _.0)) '_.2)))
        '(lambda (_.0) (list _.0 ((lambda (_.1) (list 'quote _.0)) '_.2))))
       (=/= ((_.0 _.1))
            ((_.0 closure))
            ((_.0 lambda))
            ((_.0 list))
            ((_.0 quote))
            ((_.1 closure))
            ((_.1 lambda))
            ((_.1 list))
            ((_.1 quote)))
       (sym _.0 _.1)
       (absento (closure _.2)))
      (((lambda (_.0) ((lambda (_.1) (list _.0 (list 'quote _.0))) '_.2))
        '(lambda (_.0) ((lambda (_.1) (list _.0 (list 'quote _.0))) '_.2)))
       (=/= ((_.0 _.1))
            ((_.0 closure))
            ((_.0 lambda))
            ((_.0 list))
            ((_.0 quote))
            ((_.1 closure))
            ((_.1 lambda))
            ((_.1 list))
            ((_.1 quote)))
       (sym _.0 _.1)
       (absento (closure _.2))))))
