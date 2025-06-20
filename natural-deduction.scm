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
  (length
   (run 3 (q)
     (debug-deducto (set) '(implies A A) q)))
  3)

