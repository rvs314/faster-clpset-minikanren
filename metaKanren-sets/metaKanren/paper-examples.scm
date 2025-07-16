;; Modified by William E. Byrd on July 16, 2025,
;; to use Chez Scheme instead of Racket.

(load "metaKanren.scm")

(test "metaKanren-paper-1"
  (run* (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (conj (== (cons a d) l1)
                                           (conj (== (cons a l3) l)
                                                 (delay (call-rel appendo d
                                                                  l2
                                                                  l3))))))))))
          (call-rel appendo '(1 2) '(3 4) z)))
     x))
  '(#(set ((1 2 3 4)))))

(test "metaKanren-paper-2"
  (run* (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (,x (== (cons a d) l1)
                                         (conj (== (cons a l3) l)
                                               (delay (call-rel appendo d
                                                                l2
                                                                l3))))))))))
          (call-rel appendo '(1 2) '(3 4) '(1 2 3 4))))
     (set '(_.))))
  '(conj))

; Gives disj in addition to conj
(test "metaKanren-paper-3"
  (run* (x)
    (eval-programo
     `(run ,(peano 1) (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (,x (== (cons a d) l1)
                                         (conj (== (cons a l3) l)
                                               (delay (call-rel appendo d
                                                                l2
                                                                l3))))))))))
          (call-rel appendo '(1 2) '(3 4) '(1 2 3 4))))
     (set '(_.))))
  '(disj
    conj))

(test "metaKanren-paper-4"
  (run 1 (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== 5 f)))
          (call-rel five z)))
     x))
  '(#(set (5))))

; Don't get what we expect when all examples are internally ground
(test "metaKanren-paper-5"
  (run 1 (e1 e2)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== ,e1 ,e2)))
          (call-rel five 5)))
     (set '(_.))))
  '(((_.0 _.0) (num _.0))))

; Aha!
(test "metaKanren-paper-6"
  (run 1 (x)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== 7 7)))
          (call-rel five 5)))
     x))
  (list (set '(_.))))

(test "metaKanren-paper-7"
  (run 3 (e1 e2)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                           (== ,e1 ,e2)))
          (call-rel five 5)))
     (set '(_.))))
  '(((_.0 _.0) (num _.0))
    (() ())
    (5 f)))

(test "metaKanren-paper-8"
  (run 1 (e1 e2)
    (eval-programo
     `(run* (z)
        (letrec-rel ((five (f)
                       (== ,e1 ,e2)))
          (call-rel five z)))
     (set 5)))
  '((5 f)))

; External grounding, extra examples to avoid overfitting, and with symbolo to
; fasten queries
(test "metaKanren-paper-9"
  (run 1 (x y w)
    (symbolo x)
    (symbolo y)
    (symbolo w)
    (eval-programo
     `(run* (z)
        (letrec-rel ((appendo (l1 l2 l)
                              (disj
                               (conj (== '() l1) (== l2 l))
                               (fresh (a)
                                 (fresh (d)
                                   (fresh (l3)
                                     (conj (== (cons a d) l1)
                                           (conj (== (cons a l3) l)
                                                 (delay (call-rel appendo ,x
                                                                  ,y
                                                                  ,w))))))))))
          (conj (call-rel appendo '(cat dog) '() '(cat dog))
                (conj (call-rel appendo '(apple) '(peach) '(apple peach))
                      (call-rel appendo '(1 2) '(3 4) z)))))
     (set '(1 2 3 4))))
  '((d l2 l3)))

; Thanks for the example, @bollu!
(test "metaKanren-paper-10"
  (run-unique* (count)
    (eval-programo
     `(run ,count (z)
        (disj (== z 1)
              (== z 2)))
     (set 1 2)))
  '(((()))
    (((_.0)) (=/= ((_.0 ()))))))

(test "metaKanren-paper-10b"
  (run-unique* (count)
    (eval-programo
     `(run ,count (z)
        (disj (== z 1)
              (== z 2)))
     (set 1 2)))
  (run-unique* (count)
    (eval-programo
     `(run ,count (z)
        (disj (== z 1)
              (== z 2)))
     (set 2 1))))

(test "metaKanren-paper-11"
  (run-unique* (count answers)
    (eval-programo `(run ,count (z)
                      (disj (== z 1)
                            (== z 2)))
                   answers))
  '((() #(set))
    ((()) #(set (1)))
    (((())) #(set (1 2)))
    ((((_.0)) #(set (1 2))) (=/= ((_.0 ()))))))
