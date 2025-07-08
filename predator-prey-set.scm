(load "llmr-set.scm")

(define rules
  (lambda (p q name delta delta^)
    (conde
      [(fresh (g1)
         (symbolo g1)
         ;;
         (absento g1 delta^)
         ;;
         (== (set `(fox hungry ,g1)) p)
         (== (set) q)
         (== 'fox-die name))]
      [(fresh (g1 g2)
         (symbolo g1) (symbolo g2)
         (absento g1 (list g2))
         ;;
         (absento g2 delta^)
         ;;         
         (== (set `(fox hungry ,g1) `(rabbit ,g2)) p)
         (== (set `(fox sated ,g1)) q)
         (== 'omnomnom name))]
      [(fresh (g1)
         (symbolo g1)
         (== (set `(fox sated ,g1)) p)
         (== (set `(fox hungry ,g1)) q)
         (== 'get-hungry name))]
      [(fresh (h1 h2 g1 g2 g3)
         (symbolo g1) (symbolo g2) (symbolo g3)
         (absento g1 (list g2 g3))
         (absento g2 (list g3))
         ;;
         (absento g3 delta)
         ;;
         (== (set `(fox ,h1 ,g1) `(fox ,h2 ,g2)) p)
         (== (set `(fox ,h1 ,g1) `(fox ,h2 ,g2) `(fox hungry ,g3)) q)
         (== 'fox-multiply name))]
      [(fresh (g1 g2 g3 g4)
         (symbolo g1) (symbolo g2) (symbolo g3) (symbolo g4)
         (absento g1 (list g2 g3 g4))
         (absento g2 (list g3 g4))
         (absento g3 (list g4))
         ;;
         (absento g3 delta)
         (absento g4 delta)
         ;;
         (== (set `(rabbit ,g1) `(rabbit ,g2)) p)
         (== (set `(rabbit ,g1) `(rabbit ,g2) `(rabbit ,g3) `(rabbit ,g4)) q)
         (== 'rabbit-multiply name))])))

(test "predator-prey-1"
  (run-unique* (next-state name)
    (fresh (initial-state)
      (== (set `(fox hungry f1) `(fox hungry f2) `(rabbit r1) `(rabbit r2)) initial-state)
      (step initial-state next-state name)))
  '(((#(set
        ((fox hungry _.0)
         (fox hungry f1)
         (fox hungry f2)
         (rabbit r1)
         (rabbit r2)))
      fox-multiply)
     (=/= ((_.0 f1)) ((_.0 f2)) ((_.0 fox)) ((_.0 hungry))
          ((_.0 r1)) ((_.0 r2)) ((_.0 rabbit)))
     (sym _.0))
    ((#(set
        ((fox hungry f1) (fox hungry f2) (rabbit _.0) (rabbit _.1)
         (rabbit r1) (rabbit r2)))
      rabbit-multiply)
     (=/= ((_.0 _.1)) ((_.0 f1)) ((_.0 f2)) ((_.0 fox)) ((_.0 hungry))
          ((_.0 r1)) ((_.0 r2)) ((_.0 rabbit)) ((_.1 f1)) ((_.1 f2))
          ((_.1 fox)) ((_.1 hungry)) ((_.1 r1)) ((_.1 r2))
          ((_.1 rabbit)))
     (sym _.0 _.1))
    (#(set ((fox hungry f1) (fox sated f2) (rabbit r1)))
     omnomnom)
    (#(set ((fox hungry f1) (fox sated f2) (rabbit r2)))
     omnomnom)
    (#(set ((fox hungry f1) (rabbit r1) (rabbit r2)))
     fox-die)
    (#(set ((fox hungry f2) (fox sated f1) (rabbit r1)))
     omnomnom)
    (#(set ((fox hungry f2) (fox sated f1) (rabbit r2)))
     omnomnom)
    (#(set ((fox hungry f2) (rabbit r1) (rabbit r2)))
     fox-die)))

(test "predator-prey 2"
  (run-unique 2 (tr)
    (fresh (final rest diff initial-state g5 g6 g7 g8 g9)
      (== (set `(fox hungry f1) `(fox hungry f2) `(rabbit r1) `(rabbit r2)) initial-state)
      (== `(,initial-state . ,rest) tr)
      (split (set `(fox sated ,g5) `(fox sated ,g6) `(fox sated ,g7) `(rabbit ,g8) `(rabbit ,g9)) final diff)
      (step* initial-state final rest)))
  '((#(set ((fox hungry f1) (fox hungry f2) (rabbit r1) (rabbit r2)))
     (fox-die #(set ((fox hungry f2) (rabbit r1) (rabbit r2))))
     (omnomnom #(set ((fox sated f2) (rabbit r2)))))
    (#(set ((fox hungry f1) (fox hungry f2) (rabbit r1) (rabbit r2)))
     (omnomnom #(set ((fox hungry f2) (fox sated f1) (rabbit r2)))))))
