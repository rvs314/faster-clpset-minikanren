;; Initial Loads
(begin
  ;; Chez-specific code to get better debug info.
  (eval-when (eval load)
    (optimize-level 0)
    (debug-level 3))

  (load "mk-vicare.scm")
  (load "sets.scm")
  (load "mk.scm")
  (load "test-check.scm"))

(define (deducto gamma prop)
  (conde
    ((symbolo prop)
     (ino prop gamma))
    ((fresh (a b)
       (== `(implies ,a ,b) prop)
       (deducto (set-cons a gamma) b)))
    ((fresh (a)
       (deducto gamma `(implies ,a ,prop))
       (deducto gamma a)))))

(define (debug-deducto gamma prop d)
  (conde
    ((symbolo prop)
     (ino prop gamma)
     (== `(t-var (,gamma entails ,prop)) d))
    ((fresh (a b d-rec)
       (== `(implies ,a ,b) prop)
       (== `(t-abs ,d-rec (,gamma entails ,prop)) d)
       (debug-deducto (set-cons a gamma) b d-rec)))
    ((fresh (a d-rec-implies d-rec-a)
       (== `(t-app ,d-rec-implies ,d-rec-a (,gamma entails ,prop)) d)
       (debug-deducto gamma `(implies ,a ,prop) d-rec-implies)
       (debug-deducto gamma a d-rec-a)))))

(test "implies-0"
  (run 2 (q)
    (deducto (set) '(implies A A)))
  '(_.0 _.0))

(test "implies-debug-0"
  (run 3 (q)
    (debug-deducto (set) '(implies A A) q))
  '((t-abs
      (t-var (#(set (A)) entails A))
      (#(set) entails (implies A A)))
    (t-abs
      (t-app
        (t-abs (t-var (#(set (A)) entails A)) (#(set (A)) entails (implies A A)))
        (t-var (#(set (A)) entails A)) (#(set (A)) entails A))
      (#(set) entails (implies A A)))
    ((t-app
       (t-abs
         (t-abs (t-var (#(set (A (implies _.0 _.0))) entails A)) (#(set ((implies _.0 _.0))) entails (implies A A)))
         (#(set) entails (implies (implies _.0 _.0) (implies A A))))
       (t-abs
         (t-var (#(set (_.0)) entails _.0))
         (#(set) entails (implies _.0 _.0))) (#(set) entails (implies A A))) (sym _.0))))

