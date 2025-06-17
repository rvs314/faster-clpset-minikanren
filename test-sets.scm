
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
        `(((_.0 ,(set-cons 1 _.0)) (set _.0))))

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
        `((,(set-cons 3 _.0) (set _.0))))

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
        `((_.0 (set _.0))
          (,(set-cons 1 '_.0) (set _.0))))

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
        '(1 _.0))

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
        `(((_.0 _.1 _.0 _.1) (set _.1))
          ((_.0 _.1 _.0 ,(set* '_.0 '_.1)) (set _.1))
          ((_.0 ,(set* '_.0 '_.1) _.0 _.1) (set _.1))
          ((_.0 ,(set* '_.1 '_.2) _.1 ,(set* '_.0 '_.2)) (set _.2)))))

;; occurs-check
(begin
  (test "Subterm fails occurs-check"
        (run* (p)
          (== p (set p)))
        '())
  (test "\"recursive\" set tails are equivalent to fresh variables"
        (run* (p)
          (== p (set* 1 p)))
        `((,(set* 1 _.0) (set _.0))))
  (test "Relay race"
         (run* (p q r s)
           (== s p)
           (== p (set* 1 q))
           (== q (set* 2 r))
           (== r (set* 3 s)))
         `(((,(set* 1 2 3 _.0) ,(set* 1 2 3 _.0) ,(set* 1 2 3 _.0) ,(set* 1 2 3 _.0))
            (set _.0)))))

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
        '((_.0 (set _.0) (∉ (1 _.0)))))

  (test "Add two constraint to a variable"
        (run* (q)
          (!ino 1 q)
          (!ino 2 q))
        '((_.0 (set _.0) (∉ (1 _.0) (2 _.0)))))

  (test "Add two constraint to two variables"
        (run* (p q)
          (!ino 1 p)
          (!ino 2 q))
        '(((_.0 _.1) (set _.0 _.1) (∉ (1 _.0) (2 _.1)))))

  (test "Add a variable constraint to a variable"
        (run* (q)
          (fresh (f)
            (!ino f q)))
        '((_.0 (set _.0)))))

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

;; Disjo
(begin
  (test "Empty set is always disjoint"
        (run* (q)
          (disjo (set) (set 1 2 3)))
        '(_.0))
  (test "Empty set is always disjoint (2)"
        (run* (q)
          (disjo (set 1 2 3) (set)))
        '(_.0))
  (test "Concrete disjoint sets become =/= constraints"
        (run* (a b c d)
          (disjo (set a b) (set c d)))
        '(((_.0 _.1 _.2 _.3)
           (=/= ((_.0 _.2)) ((_.0 _.3)) ((_.1 _.2)) ((_.1 _.3))))))
  (test "Abstract disjoint sets become !ino constraints"
        (run* (q)
          (disjo (set 1 2 3) q))
        '((_.0 (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))))
  (test "Completely abstract disjo"
        (run* (p q)
          (disjo p q))
        '(((_.0 _.1) (set _.0 _.1) (∥ (_.0 _.1)))))
  (test "Disjo before unification"
    (run* (p q)
      (disjo p q)
      (== p (set 1 2 3)))
    `(((,(set 1 2 3) _.0) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0))))))

;; Uniono
(begin
  (test "Union with the empty set is the identity"
        (run* (q)
          (uniono (set) q q))
        '((_.0 (set _.0))))
  (test "Union with the empty set is the identity (2)"
        (run* (q)
          (uniono q (set) q))
        '((_.0 (set _.0))))
  (test "Union over concrete sets"
        (run* (q)
          (uniono (set 1 2) (set 3 4) q))
        `(,(set 1 2 3 4)))
  (test "Union with a known set becomes membership"
        (run* (p q)
          (uniono (set 1 2 3) p q))
        `(((_.0 ,(set* 1 2 3 _.0)) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))
          ((,(set* 1 _.0)     ,(set* 1 2 3 _.0)) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))
          ((,(set* 2 _.0)     ,(set* 1 2 3 _.0)) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))
          ((,(set* 1 2 _.0)   ,(set* 1 2 3 _.0)) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))
          ((,(set* 3 _.0)     ,(set* 1 2 3 _.0)) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))
          ((,(set* 1 3 _.0)   ,(set* 1 2 3 _.0)) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))
          ((,(set* 2 3 _.0)   ,(set* 1 2 3 _.0)) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))
          ((,(set* 1 2 3 _.0) ,(set* 1 2 3 _.0)) (set _.0) (∉ (1 _.0) (2 _.0) (3 _.0)))))
  (test "Union over fresh variables"
        (run* (p q r)
          (uniono p q r))
        '(((_.0 _.1 _.2) (set _.0 _.1 _.2) (∪₃ (_.0 _.1 _.2)))))
  (test "Several union constraints"
        (run* (p q r s)
          (uniono p q r)
          (uniono p r s))
        '(((_.0 _.1 _.2 _.3)
           (set _.0 _.1 _.2 _.3)
           (∪₃ (_.0 _.2 _.3) (_.0 _.1 _.2)))))
  (test "Uniono updates trigger"
        (run* (p q r)
          (== r (set 1 2 3))
          (uniono p q r))
        (run* (p q r)
          (uniono p q r)
          (== r (set 1 2 3))))

  (test "Uniono to perform subset operations"
        (run* (p q)
          (subseteqo p q))
        '(((_.0 _.1) (set _.0 _.1) (∪₃ (_.0 _.1 _.1)))))

  (test "Union + Disequality (1)"
        (run* (p q r)
          (uniono p q r)
          (=/= r (set 1 2 3)))
        `((,∅ ,∅ ,∅)
          ((_.0 _.1 _.2)
           (=/= ((_.2 ,(set 1 2 3))))
           (set _.0 _.1 _.2)
           (∉ (1 _.2))
           (∪₃ (_.0 _.1 _.2)))
          ((_.0 _.1 _.2)
           (=/= ((_.2 ,(set 1 2 3))))
           (set _.0 _.1 _.2)
           (∉ (2 _.2))
           (∪₃ (_.0 _.1 _.2)))
          ((_.0 _.1 _.2)
           (=/= ((_.2 ,(set 1 2 3))))
           (set _.0 _.1 _.2)
           (∉ (3 _.2))
           (∪₃ (_.0 _.1 _.2)))
          ((,(set* _.0 _.1) _.2 ,(set* _.0 _.3))
           (=/= ((_.0 1)) ((_.0 2)) ((_.0 3)))
           (set _.1 _.2 _.3)
           (∉ (_.0 _.1) (_.0 _.3))
           (∪₃ (_.1 _.2 _.3)))
          ((_.0 ,(set* _.1 _.2) ,(set* _.1 _.3))
           (=/= ((_.1 1)) ((_.1 2)) ((_.1 3)))
           (set _.0 _.2 _.3)
           (∉ (_.1 _.2) (_.1 _.3))
           (∪₃ (_.0 _.2 _.3)))
          ((,(set* _.0 _.1) ,(set* _.0 _.2) ,(set* _.0 _.3))
           (=/= ((_.0 1)) ((_.0 2)) ((_.0 3)))
           (set _.1 _.2 _.3)
           (∉ (_.0 _.1) (_.0 _.2) (_.0 _.3))
           (∪₃ (_.1 _.2 _.3)))
          ((,(set* _.0 _.1) _.2 ,(set* _.0 _.3))
           (=/= ((_.0 1)) ((_.0 2)) ((_.0 3)))
           (set _.1 _.2 _.3)
           (∉ (_.0 _.1) (_.0 _.3))
           (∪₃ (_.1 _.2 _.3)))
          ((_.0 ,(set* _.1 _.2) ,(set* _.1 _.3))
           (=/= ((_.1 1)) ((_.1 2)) ((_.1 3)))
           (set _.0 _.2 _.3)
           (∉ (_.1 _.2) (_.1 _.3))
           (∪₃ (_.0 _.2 _.3)))
          ((,(set* _.0 _.1) ,(set* _.0 _.2) ,(set* _.0 _.3))
           (=/= ((_.0 1)) ((_.0 2)) ((_.0 3)))
           (set _.1 _.2 _.3)
           (∉ (_.0 _.1) (_.0 _.2) (_.0 _.3))
           (∪₃ (_.1 _.2 _.3)))))
           

  (test "Union + Disequality (2)"
        (run* (p q r)
          (uniono p q r)
          (=/= q (set 1 2)))

        `(((_.0 ,∅ _.0)
           (set _.0))
          ((_.0 _.1 _.2)
           (=/= ((_.1 ,(set 1 2)))) (set _.0 _.1 _.2) (∉ (1 _.1)) (∪₃ (_.0 _.1 _.2)))
          ((_.0 _.1 _.2)
           (=/= ((_.1 ,(set 1 2))))
           (set _.0 _.1 _.2)
           (∉ (2 _.1))
           (∪₃ (_.0 _.1 _.2)))
          ((_.0 ,(set* _.1 _.2) ,(set* _.1 _.3))
           (=/= ((_.1 1)) ((_.1 2)))
           (set _.0 _.2 _.3)
           (∉ (_.1 _.0) (_.1 _.2) (_.1 _.3))
           (∪₃ (_.2 _.0 _.3)))
          ((,(set* _.0 _.1) ,(set* _.0 _.2) ,(set* _.0 _.3))
           (=/= ((_.0 1)) ((_.0 2)))
           (set _.1 _.2 _.3)
           (∉ (_.0 _.1) (_.0 _.2) (_.0 _.3))
           (∪₃ (_.2 _.1 _.3)))
          ((_.0 ,(set* _.1 _.2) ,(set* _.1 _.3))
           (=/= ((_.1 1)) ((_.1 2))) (set _.0 _.2 _.3) (∉ (_.1 _.0) (_.1 _.2) (_.1 _.3)) (∪₃ (_.2 _.0 _.3)))
          ((,(set* _.0 _.1) ,(set* _.0 _.2) ,(set* _.0 _.3))
           (=/= ((_.0 1)) ((_.0 2))) (set _.1 _.2 _.3) (∉ (_.0 _.1) (_.0 _.2) (_.0 _.3)) (∪₃ (_.2 _.1 _.3))))))

;; One-off from Will
(begin
  (test "Will's big test"
   (run* (q)
     (fresh (a b c d e f g h s1 s2 s3 s4)
       (== (list s1 s2 s3 s4) q)
       (== (set* b a) s1)
       (== (set* d c) s2)
       (== (set* f e) s3)
       (== (set* h g) s4)
       (== (cons s1 s2) (cons s3 s4))))
   `(((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1) ,(set* _.2 _.3 _.4) ,(set* _.0 _.1) ,(set* _.2 _.3 _.4)) (set _.1 _.4))
     ((,(set* _.0 _.1 _.2) ,(set* _.3 _.4) ,(set* _.0 _.1 _.2) ,(set* _.3 _.4)) (set _.2 _.4))
     ((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1) ,(set* _.2 _.3 _.4) ,(set* _.0 _.1) ,(set* _.2 _.3 _.4)) (set _.1 _.4))
     ((,(set* _.0 _.1 _.2) ,(set* _.3 _.4) ,(set* _.0 _.1 _.2) ,(set* _.3 _.4)) (set _.2 _.4))
     ((,(set* _.0 _.1) ,(set* _.2 _.3) ,(set* _.0 _.1) ,(set* _.2 _.3)) (set _.1 _.3))
     ((,(set* _.0 _.1 _.2) ,(set* _.3 _.4) ,(set* _.0 _.1 _.2) ,(set* _.3 _.4)) (set _.2 _.4))
     ((,(set* _.0 _.1) ,(set* _.2 _.3 _.4) ,(set* _.0 _.1) ,(set* _.2 _.3 _.4)) (set _.1 _.4))
     ((,(set* _.0 _.1 _.2) ,(set* _.3 _.4 _.5) ,(set* _.0 _.1 _.2) ,(set* _.3 _.4 _.5)) (set _.2 _.5)))))

;; Testing constraint inference
(begin
  (test "Constraint inference during unification"
        (run* (q)
          (== (set* 1 q) (set* 1 q)))
        `((_.0 (set _.0))
          (,(set* 1 _.0) (set _.0))
          (,(set* 1 _.0) (set _.0))
          (,(set* 1 _.0) (set _.0))))
  (test "Chat with Will"
        (run* (q)
          (== (set* 1 q) (set* 1 q))
          (symbolo q))
        '())
  (test "Constraint inference during constraint claim"
        (run* (q)
          (seto (set* 1 q)))
        '((_.0 (set _.0))))
  (test "Constraint inference only occurs MK-side"
        (run* (q)
          (let ([_ (set* 1 q)])
            succeed))
        '(_.0)))

;; Jason & Will's Test
(let ()
  (define (sets-and-tags tagged-sets tags)
    (conde
     [succeed]
     [(fresh (tagged-sets^ tag tags^ s __)
        (removeo tags tag tags^)
        (removeo tagged-sets (cons tag s) tagged-sets^)
        (uniono s __ tags)
        (sets-and-tags tagged-sets tags^))]))
  (test "Sets-and-tags"
        (run 5 (tagged-sets/no-tags tags)
          (sets-and-tags tagged-sets/no-tags tags))
        `((_.0 _.1)
          ((,(set* `(_.0 . ,(set* '_.0 '_.1)) '_.2)
            ,(set* '_.0 '_.3))
           (set _.1 _.2 _.3)
           (∉ (_.0 _.1)
              (_.0 _.3)
              ((_.0 . ,(set* '_.0 '_.1)) _.2)))
          ((,(set* `(_.0 . _.1) '_.2)
            ,(set* '_.0 '_.3))
           (set _.1 _.2 _.3)
           (∉ (_.0 _.3) ((_.0 . _.1) _.2)))
          ((,(set* `(_.0 . ,(set* '_.0 '_.1)) '_.2)
            ,(set* '_.0 '_.3))
           (set _.1 _.2 _.3)
           (∉ (_.0 _.1)
              (_.0 _.3)
              ((_.0 . ,(set* '_.0 '_.1)) _.2)))
          ((,(set* `(_.0 . ,(set* '_.0 '_.1)) `(_.2 . ,(set* _.2 _.3)) '_.4)
            ,(set* '_.0 '_.2 '_.5))
           (=/= ((_.0 _.2)))
           (set _.1 _.3 _.4 _.5)
           (∉ (_.0 _.1)
              (_.0 _.5)
              (_.2 _.3)
              (_.2 _.5)
              ((_.0 . ,(set* '_.0 '_.1)) _.4)
              ((_.2 . ,(set* '_.2 '_.3)) _.4))))))

;; Will's absento test

(test "Wills absento test"
  (run* (s1 s2)
    (absento 'clos (list s1 s2))
    (== (set-cons 42 s1) (set-cons 42 s2)))
  `(((_.0 _.0) (set _.0) (absento (clos _.0)))
    ((_.0 ,(set* 42 _.0)) (set _.0) (absento (clos _.0)))
    ((,(set* 42 _.0) _.0) (set _.0) (absento (clos _.0)))
    ((,(set* 42 _.0) ,(set* 42 _.0)) (set _.0) (absento (clos _.0)))))

;; Reficiation of [non-]atomic propagators

(begin
  (test "atomic propagator reification with absento"
        (run* (p)
          (symbolo p)
          (absento 3 p))
        '((_.0 (sym _.0))))
  (test "non-atomic propagator reification with absento"
        (run* (p)
          (seto p)
          (absento 3 p))
        '((_.0 (set _.0) (absento (3 _.0))))))


;; Absento reification

(begin
  (test "descending into set-cons"
        (run* (p)
          (absento 2 (set* 0 1 p)))
        '((_.0 (sub-absento (2 _.0)))))
  (test "direct application of sub-absento"
        (run* (p)
          (sub-absento 2 p))
        '((_.0 (sub-absento (2 _.0))))))

;; Disequality weirdness
(begin
  (test "smaller example"
        (run* (q)
          (=/= (set 3 4) (set* 4 3 q))
          (== q (set)))
        '())

  (test "indirect inequality"
        (run* (v1 v2)
          (=/= v1 v2)
          (== v1 (set 3 4))
          (fresh (s2)
            (== v2 (set* 4 3 s2))))
        `(((,(set 3 4) ,(set* 3 4 _.0 _.1))
           (=/= ((_.0 3)) ((_.0 4)))
           (set _.1))
          ((,(set 3 4) ,(set* 3 4 _.0 _.1 _.2))
           (=/= ((_.0 3)) ((_.0 4)) ((_.1 3)) ((_.1 4)))
           (set _.2)))))
