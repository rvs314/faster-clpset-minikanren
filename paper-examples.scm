(load "./all.scm")
(load "./matche.scm")
(load "./test-check.scm")

(let ()
  ;; From Fig. 3
  (defmatche (ino o l)
    [(,o (,o . ,r))]
    [(,o (,f . ,r)) (ino o r)])

  (defmatche (uniono x y x+y)
    [(() ,y ,y)]
    [((,f . ,r) ,y (,f . ,z)) (uniono r y z)])

  (defmatche (singletono o l) [(,o (,o))])

  (defmatche (subtracto s o s-o)
    [(()        ,o ())]
    [((,o . ,r) ,o ,k) (subtracto r o k)]
    [((,f . ,r) ,o (,f . ,k)) (=/= f o) (subtracto r o k)])

  (defmatche (subseto l r)
    [(() ,r)]
    [((,f . ,rst) ,r) (ino f r) (subseto rst r)])

  (defrel (set== l r)
    (subseto l r)
    (subseto r l))

  ;; From Fig. 1
  (defmatche (free-varso obj free)
    [(,x ,f) (symbolo x) (singletono x f)]
    [((λ ,x ,t1) ,f)
     (fresh (f1)
       (free-varso t1 f1)
       (subtracto f1 x f))]
    [((,t1 ,t2) ,f)
     (fresh (f1 f2)
       (free-varso t1 f1)
       (free-varso t2 f2)
       (uniono f1 f2 f))])

  (test "listset-freevars-combinator"
    (run* (q)
      (free-varso '(λ x x) q))
    '(()))

  (test "listset-freevars-singleton"
    (run* (q)
      (free-varso '(λ x y) q))
    '((y)))

  (test "listset-freevars-fail"
    (run* (q1 q2)
      (free-varso '(λ x (y y)) q1)
      (free-varso '(λ x y) q2)
      (== q1 q2))
    '())

  (test-abridged "listset-freevars-unifications"
    (run 100 (q1)
      (fresh (q)
        (free-varso '(λ x y) q)
        (set== q q1)))
    '((y) (y y) (y y y)))

  ;; Morally equivalent to the code
  ;; in Figures 4 and 5
  (load "./simple-interp.scm")

  (test-abridged "list-alist Fresh environment"
    (run 100 (env val)
      (eval-expro '(lambda (x) x) env val))
    '((() (closure x x ()))
      ((((_.0 . _.1)) (closure x x ((_.0 . _.1))))
       (=/= ((_.0 lambda))))
      ((((_.0 . _.1) (_.2 . _.3)) (closure x x ((_.0 . _.1) (_.2 . _.3))))
       (=/= ((_.0 lambda)) ((_.2 lambda)))))))

;; From Fig. 1
(defmatche (free-varso obj free)
  [(,x ,f) (symbolo x)  (singletono x f)]
  [((λ ,x ,t1) ,f)
   (fresh (f1)
     (free-varso t1 f1)
     (subtracto f1 x f))]
  [((,t1 ,t2) ,f)
   (fresh (f1 f2)
     (free-varso t1 f1)
     (free-varso t2 f2)
     (uniono f1 f2 f))])

(test "set-freevars"
  (run* (q1)
    (fresh (q)
      (free-varso '(λ x y) q)
      (== q q1)))
  '(#(set (y))))

(test "set-freevars-inference"
  (run* (q)
    (symbolo q)
    (free-varso `(λ x ,q) '#(set (y))))
  '(y))

(load "./simple-interp.scm")
(load "./mk.scm")

(set! not-in-envo freeo)

(test "constraint-alist Fresh environment"
  (run* (env val)
    (eval-expro '(lambda (x) x) env val))
  '(((_.0 (closure x x _.0)) (lst _.0) (free (lambda _.0)))))
  
