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
  (run* (a b c d)
    (absento a b)
    (lookupo c d b))
  '(((_.0 _.1 _.2 _.3)
     (=/= ((_.0 _.2)) ((_.0 _.3)))
     (lst _.1)
     (absento (_.0 _.1)))))
