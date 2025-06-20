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
;; Friday May 23, 2025
;; Updated Friday, June 20, 2025

;; Using CLP(Set) to implement a simplified version of an abstract
;; interpreter based on this blog post:
;;
;; https://matt.might.net/articles/intro-static-analysis/

;; Peano numeral / abstract sign set relation
(define (alphao n an)
  (conde
    ((== 'z n) (== (set 0) an))
    ((fresh (n-1)
       (== `(s ,n-1) n)
       (== (set '+) an)))))

(define (+-abstracto an1 an2 an-sum)
  (conde
    ((== (set 0) an1) (== (set 0) an2) (== (set 0) an-sum))
    ((== (set 0) an1) (== (set '+) an2) (== (set '+) an-sum))
    ((== (set '+) an1) (== (set 0) an2) (== (set '+) an-sum))
    ((== (set '+) an1) (== (set '+) an2) (== (set '+) an-sum))
    ((== (set 0) an1) (== (set 0 '+) an2) (== (set 0 '+) an-sum))
    ((== (set '+) an1) (== (set 0 '+) an2) (== (set '+) an-sum))
    ((== (set 0 '+) an1) (== (set 0) an2) (== (set 0 '+) an-sum))
    ((== (set 0 '+) an1) (== (set '+) an2) (== (set '+) an-sum))
    ((== (set 0 '+) an1) (== (set 0 '+) an2) (== (set 0 '+) an-sum))))

(define (*-abstracto an1 an2 an-prod)
  (conde
    ((== (set 0) an1) (== (set 0) an2) (== (set 0) an-prod))
    ((== (set 0) an1) (== (set '+) an2) (== (set '0) an-prod))
    ((== (set '+) an1) (== (set 0) an2) (== (set '0) an-prod))
    ((== (set '+) an1) (== (set '+) an2) (== (set '+) an-prod))
    ((== (set 0) an1) (== (set 0 '+) an2) (== (set 0) an-prod))
    ((== (set '+) an1) (== (set 0 '+) an2) (== (set 0 '+) an-prod))
    ((== (set 0 '+) an1) (== (set 0) an2) (== (set 0) an-prod))
    ((== (set 0 '+) an1) (== (set '+) an2) (== (set 0 '+) an-prod))
    ((== (set 0 '+) an1) (== (set 0 '+) an2) (== (set 0 '+) an-prod))))

(define (aevalo expr aval)
  (expr-aevalo expr '() aval))

(define (expr-aevalo expr aenv aval)
  (conde
    ((symbolo expr) (lookupo expr aenv aval))
    ((fresh (pn)
       ;; pn = Peano numeral
       (== `(num ,pn) expr)
       (alphao pn aval)))
    ((fresh (e1 e2)
       (== `(= ,e1 ,e2) expr)
       (== (set 0 '+) aval)))
    ((fresh (e1 e2 av1 av2)
       (== `(+ ,e1 ,e2) expr)
       (+-abstracto av1 av2 aval)
       (expr-aevalo e1 aenv av1)
       (expr-aevalo e2 aenv av2)))
    ((fresh (e1 e2 av1 av2)
       (== `(* ,e1 ,e2) expr)
       (*-abstracto av1 av2 aval)
       (expr-aevalo e1 aenv av1)
       (expr-aevalo e2 aenv av2)))))


(define (evalo expr val)
  (expr-evalo expr '() val))

(define (expr-evalo expr env val)
  (conde
    ((fresh (pn)
       ;; pn = Peano numeral
       (== `(num ,pn) expr)
       (== `(num ,pn) val)))
    ((symbolo expr) (lookupo expr env val))
    ((fresh (e1 e2 pn1 pn2)
       (== `(= ,e1 ,e2) expr)
       (conde
         ((== pn1 pn2) (== #t val))
         ((=/= pn1 pn2) (== #f val)))
       (expr-evalo e1 env `(num ,pn1))
       (expr-evalo e2 env `(num ,pn2))))
    ((fresh (e1 e2 pn1 pn2 pn-sum)
       (== `(+ ,e1 ,e2) expr)
       (== `(num ,pn-sum) val)
       (expr-evalo e1 env `(num ,pn1))
       (expr-evalo e2 env `(num ,pn2))
       (+o pn1 pn2 pn-sum)))
    ((fresh (e1 e2 pn1 pn2 pn-prod)
       (== `(* ,e1 ,e2) expr)
       (== `(num ,pn-prod) val)
       (expr-evalo e1 env `(num ,pn1))
       (expr-evalo e2 env `(num ,pn2))
       (*o pn1 pn2 pn-prod)))))

(define (+o pn1 pn2 pn-sum)
  (conde
    ((== 'z pn1) (== pn2 pn-sum))
    ((fresh (pn1-1)
       (== `(s ,pn1-1) pn1)
       (+o pn1-1 `(s ,pn2) pn-sum)))))

(define (*o pn1 pn2 pn-prod)
  (conde
    ((== 'z pn1) (== 'z pn-prod))
    ((fresh (pn1-1 res-pn)
       (== `(s ,pn1-1) pn1)
       (*o pn1-1 pn2 res-pn)
       (+o res-pn pn2 pn-prod)))))

(define (lookupo x env val)
  (fresh (y v env^)
    (symbolo x)
    (symbolo y)
    (== `((,y . ,v) . ,env^) env)
    (conde
      ((== x y) (== v val))
      ((=/= x y) (lookupo x env^ val)))))


(test "concrete-evalo-1"
  (run 10 (expr val) (evalo expr val))
  '(((num _.0) (num _.0))
    ((= (num _.0) (num _.0)) #t)
    (((= (num _.0) (num _.1)) #f) (=/= ((_.0 _.1))))
    ((+ (num z) (num _.0)) (num _.0))
    ((* (num z) (num _.0)) (num z))
    ((+ (num (s z)) (num _.0)) (num (s _.0)))
    ((* (num (s z)) (num _.0)) (num _.0))
    ((+ (num (s (s z))) (num _.0)) (num (s (s _.0))))
    ((+ (num (s (s (s z)))) (num _.0)) (num (s (s (s _.0)))))
    ((= (num _.0) (+ (num z) (num _.0))) #t)))

(test "abstract-evalo-1"
  (run 10 (expr val) (aevalo expr val))
  `(((num z) ,(set 0))
    ((= _.0 _.1) ,(set 0 '+))
    ((num (s _.0)) ,(set '+))
    ((+ (num z) (num z)) ,(set 0))
    ((* (num z) (num z)) ,(set 0))
    ((+ (num z) (num (s _.0))) ,(set '+))
    ((* (num z) (num (s _.0))) ,(set 0))
    ((+ (num z) (+ (num z) (num z))) ,(set 0))
    ((* (num z) (+ (num z) (num z))) ,(set 0))
    ((+ (num z) (* (num z) (num z))) ,(set 0))))

(test "concrete-and-abstract-evalo-1"
  (run 10 (expr val aval)
    (== (set '+) aval)
    (aevalo expr aval)
    (evalo expr val))
  `(((num (s _.0))
     (num (s _.0))
     ,(set '+))
    ((* (num (s z)) (num (s _.0)))
     (num (s _.0))
     ,(set '+))
    ((* (num (s (s z))) (num (s z)))
     (num (s (s z)))
     ,(set '+))
    ((+ (num z) (num (s _.0)))
     (num (s _.0))
     ,(set '+))
    ((* (num (s (s z))) (num (s (s z))))
     (num (s (s (s (s z)))))
     ,(set '+))
    ((* (num (s (s z))) (num (s (s (s z)))))
     (num (s (s (s (s (s (s z)))))))
     ,(set '+))
    ((* (num (s (s z))) (num (s (s (s (s z))))))
     (num (s (s (s (s (s (s (s (s z)))))))))
     ,(set '+))
    ((* (num (s (s z))) (num (s (s (s (s (s z)))))))
     (num (s (s (s (s (s (s (s (s (s (s z)))))))))))
     ,(set '+))
    ((* (num (s (s z))) (num (s (s (s (s (s (s z))))))))
     (num (s (s (s (s (s (s (s (s (s (s (s (s z)))))))))))))
     ,(set '+))
    ((* (num (s (s (s z)))) (num (s z)))
     (num (s (s (s z))))
     ,(set '+))))
