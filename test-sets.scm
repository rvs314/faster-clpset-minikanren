(load "./test-check.scm")
(load "./sets.scm")
(load "./mk-vicare.scm")
(load "./mk.scm")
;; FIXME: call test-sets from test-all, not other way around
(load "./test-all.scm")

(printf "set-tests~%")

;; Basic set constructs (no constraints)

(test "set equality"
      (set 0 1 2)
      (set 0 1 2))

(test "set cons"
      (set 0 1 2)
      (set-cons 0 (set-cons 1 (set-cons 2 ∅))))

(test "set->list"
      (set->list (set 0 1 2))
      (list 0 1 2))

(test "list->set"
      (list->set (list 0 1 2))
      (set 0 1 2))

;; Basic MK constructs using sets

(test "set-null unifies correctly"
      (run* (q)
        (== (cons (cons 1 ∅) ∅)
            (cons (cons 1 ∅) ∅))
        (== ∅ ∅)
        (== q ∅))
      (list ∅))


(test "seto of set-null works"
      (run* (q)
        (seto ∅))
      '(_.0))

;; Currently failing: set unification not implemented yet
(test "seto of set-cons works"
      (run* (q)
        (seto (set-cons 1 ∅)))
      '(_.0))

(test "seto of variable works"
      (run* (q)
        (seto q))
      '((_.0 (set _.0))))

(test "set tail must be a set"
      (run* (q)
        (seto q)
        (== q (set-cons 1 2)))
      '())

(test "set tail may be a variable"
      (run* (p q)
        (seto q)
        (== q (set-cons 1 p)))
      `(((_.0 ,(set-cons 1 '_.0)) (set _.0))))

(test "sets aren't other things"
      (run* (q)
        (seto q)
        (conde
         [(symbolo q)]
         [(numbero q)]
         [(stringo q)]))
      '())

;; (test "set-pair unifies correctly"
;;       (run* (q)
;;         (== q (set-cons 1 2))
;;         (== (set-cons 1 2) q))
;;       (list (set-cons 1 2)))

