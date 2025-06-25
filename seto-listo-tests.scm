#|
This is a relatively mechanical translation (all but 3 tests were just
transformed with find-and-replace) from the symbolo-numbero tests.

I added a few onto the end to test listo propagation as well.
|#

(test "seto-listo-1"
  (run* (q) (seto q) (listo q))
  '())

(test "seto-listo-2"
  (run* (q) (listo q) (seto q))
  '())

(test "seto-listo-3"
  (run* (q)
    (fresh (x)
      (listo x)
      (seto x)))
  '())

(test "seto-listo-4"
  (run* (q)
    (fresh (x)
      (seto x)
      (listo x)))
  '())

(test "seto-listo-5"
  (run* (q)
    (listo q)
    (fresh (x)
      (seto x)
      (== x q)))
  '())

(test "seto-listo-6"
  (run* (q)
    (seto q)
    (fresh (x)
      (listo x)
      (== x q)))
  '())

(test "seto-listo-7"
  (run* (q)
    (fresh (x)
      (listo x)
      (== x q))
    (seto q))
  '())

(test "seto-listo-7"
  (run* (q)
    (fresh (x)
      (seto x)
      (== x q))
    (listo q))
  '())

(test "seto-listo-8"
  (run* (q)
    (fresh (x)
      (== x q)
      (seto x))
    (listo q))
  '())

(test "seto-listo-9"
  (run* (q)
    (fresh (x)
      (== x q)
      (listo x))
    (seto q))
  '())

(test "seto-listo-10"
  (run* (q)
    (seto q)
    (fresh (x)
      (listo x)))
  '((_.0 (set _.0))))

(test "seto-listo-11"
  (run* (q)
    (listo q)
    (fresh (x)
      (seto x)))
  '((_.0 (lst _.0))))

(test "seto-listo-12"
  (run* (q)
    (fresh (x y)
      (seto x)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (set _.0))))

(test "seto-listo-13"
  (run* (q)
    (fresh (x y)
      (listo x)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (lst _.0))))

(test "seto-listo-14"
  (run* (q)
    (fresh (x y)
      (listo x)
      (seto y)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (lst _.0) (set _.1))))

(test "seto-listo-15"
  (run* (q)
    (fresh (x y)
      (listo x)
      (== `(,x ,y) q)
      (seto y)))
  '(((_.0 _.1) (lst _.0) (set _.1))))

(test "seto-listo-16"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (listo x)
      (seto y)))
  '(((_.0 _.1) (lst _.0) (set _.1))))

(test "seto-listo-17"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (listo x)
      (seto y))
    (fresh (w z)
      (== `(,w ,z) q)))
  '(((_.0 _.1) (lst _.0) (set _.1))))

(test "seto-listo-18"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (listo x)
      (seto y))
    (fresh (w z)
      (== `(,w ,z) q)
      (== w '(5))))
  '((((5) _.0) (set _.0))))

(test "seto-listo-19"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (listo x)
      (seto y))
    (fresh (w z)
      (== (set 'b 'a) z)
      (== `(,w ,z) q)))
  '(((_.0 #(set (a b))) (lst _.0))))

(test "seto-listo-20"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (listo x)
      (seto y))
    (fresh (w z)
      (== `(,w ,z) q)
      (== (set 'a) z)))
  '(((_.0 #(set (a))) (lst _.0))))

(test "seto-listo-21"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (=/= ((_.0 5) (_.1 a))))))

(test "seto-listo-22"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= `(5 a) q)
      (seto x)))
  '(((_.0 _.1) (set _.0))))

(test "seto-listo-23"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (seto x)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (set _.0))))

(test "seto-listo-24"
  (run* (q)
    (fresh (x y)
      (seto x)
      (== `(,x ,y) q)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (set _.0))))

(test "seto-listo-25"
  (run* (q)
    (fresh (x y)
      (=/= `(5 a) q)
      (seto x)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (set _.0))))

(test "seto-listo-26"
  (run* (q)
    (fresh (x y)
      (=/= `(5 a) q)
      (== `(,x ,y) q)
      (seto x)))
  '(((_.0 _.1) (set _.0))))

(test "seto-listo-27"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= `(5 a) q)
      (listo y)))
  '(((_.0 _.1) (lst _.1))))

(test "seto-listo-28"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (listo y)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (lst _.1))))

(test "seto-listo-29"
  (run* (q)
    (fresh (x y)
      (listo y)
      (== `(,x ,y) q)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (lst _.1))))

(test "seto-listo-30"
  (run* (q)
    (fresh (x y)
      (=/= `(5 a) q)
      (listo y)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (lst _.1))))

(test "seto-listo-31"
  (run* (q)
    (fresh (x y)
      (=/= `(5 a) q)
      (== `(,x ,y) q)
      (listo y)))
  '(((_.0 _.1) (lst _.1))))

(test "seto-listo-32"
  (run* (q)
    (fresh (x y)
      (=/= `(,x ,y) q)
      (listo x)
      (seto y)))
  '(_.0))

(test "seto-listo-33"
  (run* (q)
    (fresh (x y)
      (listo x)
      (=/= `(,x ,y) q)
      (seto y)))
  '(_.0))

(test "seto-listo-34"
  (run* (q)
    (fresh (x y)
      (listo x)
      (seto y)
      (=/= `(,x ,y) q)))
  '(_.0))

(test "lists-propagate"
  (run* (q r s)
    (listo q)
    (== q (cons 1 r))
    (== r (cons 1 s)))
  '((((1 1 . _.0) (1 . _.0) _.0) (lst _.0))))

(test "sets-propagate"
  (run* (q r s)
    (seto q)
    (== q (set-cons 1 r))
    (== r (set-cons 1 s)))
  '(((#(set (1) _.0) #(set (1) _.0) _.0) (set _.0))))

(test "list-constructors"
  (run* (p)
    (listo (list* 1 2 3 p))
    (listo `(a b c . ,p)))
  '((_.0 (lst _.0))))

(test "set-literals"
  (run* (p)
    (seto (set* 1 2 3 p))
    (seto `#(set (a b c) ,p)))
  '((_.0 (set _.0))))
