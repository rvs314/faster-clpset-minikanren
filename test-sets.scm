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

(define-syntax run-unique*
  (syntax-rules ()
    [(run-unique* (v ...)
       g ...)
     (unique (run* (v ...)
               g ...))]))

;; Set constructors
(begin
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
        (set 0 1 2)))

;; Seto
(begin 
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
        '()))

;; Ino
(begin 
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
        `(,(set-cons 3 '_.0)))

  (test "ino works on constraint update"
        (run* (q)
          (ino 3 q)
          (== q ∅))
        '())

  (test "ino forces unification eventually"
        (run* (q r)
          (ino 3 q)
          (== q (set-cons r ∅)))
        `((,(set 3) 3) (,(set 3) 3)))

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
        `(_.0 ,(set-cons 1 '_.0)))

  (test "ino generates duplicates (2)"
        (run* (q)
          (ino q (set 1 1 1 1)))
        '(1 1 1 1)))

;; Set unification
(begin
  
  (test "literal sets unify correctly"
        (run* (q)
          (== (set 1 2) (set 1 2)))
        '(_.0))

  (test "partially instantiated sets unify correctly"
        (run-unique* (q)
          (== (set 1 q) (set 1 q)))
        '(1 _.0)))

(test "partially instantiated sets unify correctly (2)"
      (run-unique* (p q r)
        (== (set p q r) (set p q r)))
      '((_.0 _.0 _.0)
        (_.0 _.0 _.1)
        (_.0 _.1 _.0)
        (_.0 _.1 _.1)
        (_.0 _.1 _.2)))

(test "Example from paper"
      (run* (X Y R S)
        (== (set-cons X Y) (set-cons R S)))
      `((_.0 _.1 _.0 _.1)
        (_.0 _.1 _.0 ,(set* '_.0 '_.1))
        (_.0 ,(set* '_.0 '_.1) _.0 _.1)
        (_.0 ,(set* '_.1 '_.2) _.1 ,(set* '_.0 '_.2))))

;; Set with disequality
(begin
  (test "sets with disequality"
        (run* (P)
          (=/= (set P) (set P)))
        '())

  (test "sets with disequality (2)"
        (run* (P)
          (=/= (set P) (set P 1)))
        '((_.0 (=/= ((_.0 1)))))))

;; Will's CLP(Set) tests
(begin
  (test "w1a"
    (run-unique* (q)
      (== (set 1) (set 1 1 1 1 1)))
    '(_.0))

  (test "w1b"
    (run-unique* (q)
      (== (set 1 1 1 1 1) (set 1)))
    '(_.0))

  (test "w2"
    (run-unique* (q)
      (== ∅ ∅))
    '(_.0))

  (test "w3"
    (run-unique* (q)
      (== (set 1 2) (set 2 1)))
    '(_.0))

  (test "w4"
    (run-unique* (q)
      (== (set 1 2) (set q 1)))
    '(2))

  (test "w5"
    (run-unique* (q)
      (== (set 2 1 2) (set 1 q 1)))
    '(2))

  (test "w6"
    (run-unique* (q)
      (fresh (x y)
        (== (list x y) q)
        (== (set x 2) (set y 1))))
    '((1 2)))

  (test "d1"
    (run-unique* (q)
      (=/= ∅ ∅))
    '())

  (test "d2"
    (run-unique* (q)
      (=/= (set 1 2 1) (set 2 2 1)))
    '())

  (test "d3a"
    (run-unique* (q)
      (fresh (s1 s2)
        (== (list s1 s2) q)
        (seto s1)
        (seto s2)
        (== s1 s2)
        (=/= s1 s2)))
    '())

  (test "d3b"
    (run-unique* (q)
      (fresh (s1 s2)
        (== (list s1 s2) q)
        (seto s1)
        (seto s2)
        (=/= s1 s2)
        (== s1 s2)))
    '())

  (test "d3c"
    (run-unique* (q)
      (fresh (s1 s2)
        (== (list s1 s2) q)
        (=/= s1 s2)
        (seto s1)
        (seto s2)
        (== s1 s2)))
    '())

  (test "d4"
    (run-unique* (q)
      (fresh (x y)
        (== (list x y) q)
        (=/= (set x 2) (set 1 2))))
    ;; Note, this isn't the result Will got,
    ;; as faster-mk has a better reifier,
    ;; so one less redundant choice
    '(((_.0 _.1) (=/= ((_.0 1))))))

  (test "s1a"
    (run-unique* (q)
      (== ∅ (set ∅)))
    '())

  (test "s1b"
    (run-unique* (q)
      (== (set ∅) ∅))
    '())

  (test "s2"
    (run-unique* (q)
      (fresh (s1 s2)
        (== (list s1 s2) q)
        (seto s1)
        (seto s2)
        (== (set* s2 s1) s2)))
    '())

  (test "s3"
    (run-unique* (q)
      (fresh (s1 s2 s3)
        (== (list s1 s2 s3) q)
        (seto s1)
        (seto s2)
        (seto s3)
        (== (set* s2 s1) s3)))
    `(((_.0 _.1 ,(set* '_.1 '_.0)) (set _.0 _.1))))

  (test "s4"
    (run-unique* (q)
      (== (set q) ∅))
    '())

  (test "s5"
    (run-unique* (q)
      (seto q)
      (== (set q) q))
    '())

  (test "s6"
    (run-unique* (q)
      (== (set q) q))
    '()))

;; sets with absento
(begin
  (test "Ground value absent from ground set"
        (run* (q)
          (absento 1 (set 2 3)))
        '(_.0))
  (test "Ground value with a variable set"
        (run* (q)
          (absento (set) (set 0 q)))
        `((_.0 (absento (,∅ _.0))))))

;; Sets with !ino
(begin
  (test "Nothing is in the empty set"
        (run* (q)
          (!ino q ∅))
        '(_.0))

  (test "Variable ∉ Ground Set"
        (run* (q)
          (!ino q (set 1 2 3)))
        '((_.0 (=/= ((_.0 1)) ((_.0 2)) ((_.0 3))))))

  (test "The singleton set contains something"
        (run* (q)
          (!ino q (set q)))
        '())

  (test "Add a constraint to a variable"
        (run* (q)
          (!ino 1 q))
        '((_.0 (∌ _.0 1))))

  (test "Add a variable constraint to a variable"
        (run* (q)
          (fresh (f)
            (!ino f q)))
        '(_.0)))

;; Chat with Will
(begin
  (test "Absento does not apply to subsets"
        (run* (q)
          (absento (set) (set 1 2 3)))
        '(_.0))
  (test "Empty set does not contain itself"
        (run* (q)
          (!ino (set) (set)))
        '(_.0))
  (test "Singleton of Empty set contains the empty set"
        (run* (q)
          (!ino (set) (set (set))))
        '())
  (test "Singleton of Empty set contains the empty set"
        (run* (q)
          (!ino (set) (set (set))))
        '())
  (test "!ino does not recur"
        (run* (q)
          (!ino q (set (set q))))
        '(_.0))
  (test "absento does recur"
        (run* (q)
          (absento q (set (set q))))
        '()))
