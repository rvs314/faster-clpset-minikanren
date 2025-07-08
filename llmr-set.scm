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

;; William E. Byrd
;; Monday July 7, 2025

;; Revisiting this code, using CLP(Set)
;; https://github.com/webyrd/linear-logic-multiset-rewriting

;; "split p delta delta^" means
;; delta = p + delta^
;; where + is multiset union
(define split
  (lambda (p delta delta^)
    (fresh ()
      (uniono p delta^ delta)
      (disjo p delta^))))

;; "step delta delta^^ name" if:
;; delta steps to delta^^ along name
;; i.e.: 
;; - there is a rule w/name "name" : p -o q
;; - delta\p is delta^
;; - delta^ + q is delta^^
(define step
  (lambda (delta delta^^ name)
    (fresh (p q delta^)
      (rules p q name delta delta^)
      (split p delta delta^)
      (uniono delta^ q delta^^))))

;; reflexive, transitive closure of step
;; "step* delta delta^^ trace"
;; where trace is a list of the rules that fired
(define step*
  (lambda (delta delta^^ trace)
    (conde
      [(== delta delta^^) (== '() trace)]
      [(fresh (delta^ name tr)
         (== `((,name ,delta^) . ,tr) trace)
         (step delta delta^ name)
         (step* delta^ delta^^ tr))])))
