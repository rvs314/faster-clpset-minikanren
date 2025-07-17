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
;; Updated Friday June 20, 2025
;; Updated Thursday July 17, 2025

;; Tabling, implemented with CLP(Set)

;; TODO write an interpreter for a list representing `arco` edges,
;; making it possible to synthesize the edges from known result.

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


;; Similar to above, but using a set of edges to define the graph,
;; rather than a fixed `arco` relation.

;; Non-tabled version

(define (path-with-edgeso x y edge-set)
  (conde
    ((ino `(,x -> ,y) edge-set))
    ((fresh (z)
       (ino `(,x -> ,z) edge-set)
       (path-with-edgeso z y edge-set)))))

(test "path-with-edgeso"
  (run 10 (q) (path-with-edgeso 'a q (set '(a -> b)
                                          '(b -> a)
                                          '(b -> d))))
  '(b a d b a d b a d b))


;; Tabled version

(define (path-with-edges-tabledo x y edge-set table)
  (conde
    ((!ino y table)
     (ino `(,x -> ,y) edge-set))
    ((fresh (z)
       (ino `(,x -> ,z) edge-set)
       (fresh (table^)
         (!ino z table)
         (== (set-cons z table) table^)
         (path-with-edges-tabledo z y edge-set table^))))))

(test "path-with-edges-tabledo-1"
  (run* (q) (path-with-edges-tabledo 'a
                                     q
                                     (set '(a -> b)
                                          '(b -> a)
                                          '(b -> d))
                                     (set)))
  '(b a d))

(test "path-with-edges-tabledo-2"
  (run 3 (q) (path-with-edges-tabledo 'a
                                      'b
                                      q
                                      (set)))
  '((#(set ((a -> b)) _.0)
     (set _.0))
    (#(set ((_.0 -> b) (a -> _.0)) _.1)
     (=/= ((_.0 b)))
     (set _.1))
    (#(set ((_.0 -> _.1) (_.1 -> b) (a -> _.0)) _.2)
     (=/= ((_.0 _.1)) ((_.0 b)) ((_.1 b)))
     (set _.2))))
