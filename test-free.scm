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
