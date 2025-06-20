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
;; Saturday May 24, 2025


;; Tabling, implemented with CLP(Set)

;; path example (very standard) taken from section 12.4 of my dissertation

(define (patho x y)
  (conde
    ((arco x y))
    ((fresh (z)
       (arco x z)
       (patho z y)))))

(define (arco x y)
  (conde
    ((== 'a x) (== 'b y))
    ((== 'b x) (== 'a y))
    ((== 'b x) (== 'd y))))

(test "patho-not-tabled"
  (run 10 (q) (patho 'a q))
  '(b a d b a d b a d b))

(define (path-tabledo x y table)
  (conde
    ((!ino y table)
     (arco x y))
    ((fresh (z)
       (arco x z)
       (fresh (table^)
         (!ino z table)
         (== (set-cons z table) table^)
         (path-tabledo z y table^))))))

(test "patho-tabled"
  (run* (q) (path-tabledo 'a q (set)))
  '(b a d))
