;; Chez-specific code to get better debug info.
(eval-when (load)
  (optimize-level 0)
  (debug-level 3))

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

(test "ino works on set-pairs"
      (run* (q)
        (ino 3 (set 2 4 9 3 1)))
      '(_.0))

(test "ino works on set-nulls"
      (run* (q)
        (ino 3 ∅))
      '())

(test "ino works on variables"
      (run* (q)
        (ino 3 q))
      `((,(set-cons 3 '_.0) (set _.0))))

(test "ino works on constraint update"
      (run* (q)
        (ino 3 q)
        (== q ∅))
      '())

(test "ino forces unification eventually"
      (run* (q r)
        (ino 3 q)
        (== q (set-cons r ∅)))
      `((,(set 3) 3)))

(test "ino respects the pidgeonhole principle"
      (run* (q r)
        (== q (set-cons r ∅))
        (ino 3 q)
        (ino 4 q))
      '())

(test "ino respects the pidgeonhole principle (in reverse)"
      (run* (q r)
        (ino 3 q)
        (ino 4 q)
        (== q (set-cons r ∅)))
      '())

;; The duplicate generation is undesirable and should
;; be removed in the future

(test "ino generates duplicates"
      (run* (q)
        (fresh (k)
          (== k (set-cons 1 q))
          (ino 1 k)))
      `(_.0 (,(set-cons 1 '_.0) (set _.0))))

(test "ino generates duplicates (2)"
      (run* (q)
        (ino q (set 1 1 1 1)))
      '(1 1 1 1))

(test "literal sets unify correctly"
      (run* (q)
        (== (set 1 2) (set 1 2)))
      '(_.0))

(define (unique elms)
  (sort-lex (remove-duplicates elms)))

(test "partially instantiated sets unify correctly"
      (unique
       (run* (q)
         (== (set 1 q) (set 1 q))))
      '(1 _.0))
