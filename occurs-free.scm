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
;; Updated June 20, 2025

;; TODO
;; * synthesize code from a ground set of variables (including the empty set)
;; * synthesize code from a partially ground set of variables
;; * partially-specified code and partially-specified set of variables
;; * also implement `occurs-boundo`
;; * implement using lists representing sets, with recursive list
;; operations for union, etc., to show tradeoffs in representation
;; * implement variant that takes a single symbol representing an identifier:
;; (should think about different names for these variants): `(occurs-freeo x expr)`
;; Do we need sets for this version?  Or is there a simpler version?  I would think we
;; could use a simpler approach, and then maybe build up to the set-based version.

#|
Grammar for little language:

e ::=
  n |               ;; numeric literal
  x |               ;; variable
  (lambda (x) e) |  ;; abstraction
  (cons e e) |      ;; cons
  (e e)             ;; application
|#

;; put set operations before calls to recursive relations
;; (one of the main benefits of having set constraints, as
;; opposed to using recursive list operations, for example)
(define (occurs-freeo expr free-vars)
  (conde
    ((numbero expr) (== (set) free-vars))
    ((symbolo expr) (== (set expr) free-vars))
    ((fresh (x e fv)
       (symbolo x)
       (== `(lambda (,x) ,e) expr)
       (removeo fv x free-vars)      
       (occurs-freeo e fv)))
    ((fresh (e1 e2 fv1 fv2)
       (== `(cons ,e1 ,e2) expr)
       (uniono fv1 fv2 free-vars)
       (occurs-freeo e1 fv1)
       (occurs-freeo e2 fv2)))
    ((fresh (e1 e2 fv1 fv2)
       (== `(,e1 ,e2) expr)
       (uniono fv1 fv2 free-vars)
       (occurs-freeo e1 fv1)
       (occurs-freeo e2 fv2)))))

#|
;; This version is correct, I believe, but overly complicated, and
generates duplicate answers.

(define (occurs-freeo expr free-vars)
  (conde
    ((numbero expr) (== (set) free-vars))
    ((symbolo expr) (== (set expr) free-vars))
    ((fresh (x e fv)
       (symbolo x)
       (== `(lambda (,x) ,e) expr)
       ;; No reason for this case analysis, I think.
       ;; Just (removeo fv x free-vars), which handles both cases.
       (conde
         ((!ino x fv) (== fv free-vars))
         ((ino x fv) (removeo fv x free-vars)))
       (occurs-freeo e fv)))
    ((fresh (e1 e2 fv1 fv2)
       (== `(cons ,e1 ,e2) expr)
       (uniono fv1 fv2 free-vars)
       (occurs-freeo e1 fv1)
       (occurs-freeo e2 fv2)))
    ((fresh (e1 e2 fv1 fv2)
       (== `(,e1 ,e2) expr)
       (uniono fv1 fv2 free-vars)
       (occurs-freeo e1 fv1)
       (occurs-freeo e2 fv2)))))
|#

(test "occurs-freeo-0"
  (run* (q)
    (occurs-freeo '(cons (cons 3 5) (cons 42 7)) q))
  (list (set)))

(test "occurs-freeo-b"
  (run* (q)
    (occurs-freeo '(lambda (z) (cons ((lambda (a) (a z)) z) z)) q))
  (list (set)))

(test "occurs-freeo-c"
  (run* (q)
    (occurs-freeo '(lambda (z) (cons ((lambda (z) (z z)) z) z)) q))
  (list (set)))

(test "occurs-freeo-1"
  (run* (q)
    (occurs-freeo '(cons (cons x y) (cons z x)) q))
  (list (set 'x 'y 'z)))

(test "occurs-freeo-2"
  (run* (q)
    (occurs-freeo '((lambda (w) (cons y w)) (lambda (w) (cons x w))) q))
  (list (set 'x 'y)))

#;(test "occurs-freeo-2"
  (run* (q)
    (occurs-freeo '((lambda (w) (cons y w)) (lambda (w) (cons x w))) q))
  (list (set 'x 'y) (set 'x 'y) (set 'x 'y) (set 'x 'y)))

#;(test "occurs-freeo-3"
  (run-unique* (q)
    (occurs-freeo '((lambda (w) (cons y w)) (lambda (w) (cons x w))) q))
  (list (set 'x 'y)))

(test "occurs-freeo-synth-1"
  (run* (q)
    (occurs-freeo `(lambda (,q) (cons z z)) (set)))
  '(z))

(test "occurs-freeo-synth-2"
  (run* (a b)
    (occurs-freeo `(lambda (,a) (lambda (,b) (cons w z))) (set)))
  '((z w) (w z)))

(test "occurs-freeo-synth-3"
  (run 10 (expr)
    (occurs-freeo expr (set)))
  '((_.0
     (num _.0))
    ((lambda (_.0) _.0)
     (sym _.0))
    ((cons _.0 _.1)
     (num _.0 _.1))
    ((_.0 _.1)
     (num _.0 _.1))
    ((cons _.0 (lambda (_.1) _.1))
     (num _.0) (sym _.1))
    ((_.0 (lambda (_.1) _.1))
     (num _.0) (sym _.1))
    ((cons (lambda (_.0) _.0) _.1)
     (num _.1) (sym _.0))
    (((lambda (_.0) _.0) _.1)
     (num _.1) (sym _.0))
    ((lambda (_.0) (cons _.0 _.1))
     (num _.1) (sym _.0))
    ((lambda (_.0) (_.0 _.1))
     (num _.1) (sym _.0))))

(test "occurs-freeo-synth-4"
  (run-unique 20 (expr)
    (occurs-freeo expr (set 'a 'b 'c)))
  '((a (b c))
    (a (cons b c))
    (cons a (b c))
    (cons a (cons b c))
    (cons (a b) c)
    (cons (cons a b) c)
    ((a b) c)
    ((cons a b) c)
    ((cons (a b) (c _.0))
     (num _.0))
    ((cons (a b) (cons c _.0))
     (num _.0))
    ((cons (cons a b) (c _.0))
     (num _.0))
    ((cons (cons a b) (c (lambda (_.0) _.0)))
     (sym _.0))
    ((cons (cons a b) (cons c _.0))
     (num _.0))
    ((cons (cons a b) (cons c (lambda (_.0) _.0)))
     (sym _.0))
    (((a b) (c _.0))
     (num _.0))
    (((a b) (cons c _.0))
     (num _.0))
    (((cons a b) (c _.0))
     (num _.0))
    (((cons a b) (c (lambda (_.0) _.0)))
     (sym _.0))
    (((cons a b) (cons c _.0))
     (num _.0))
    (((cons a b) (cons c (lambda (_.0) _.0)))
     (sym _.0))))

;; Idea--what if we accumulate keywords such as `lambda` and `cons` in
;; a set as we traverse the expression.?  Could we use sets to encode
;; `presento`-like behavior?  Or perhaps that a number (any number) or
;; a specific number must be encountered.  I guess the challenge is
;; that the expression must still be sufficiently ground in order to
;; ensure this in general.  Combine with delayed goal?  Delay
;; enumeration until floundering?  Tree automata?

(test "occurs-freeo-synth-5"
  (run-unique 20 (expr)
    ;; When most every answer is one or more `cons` calls it becomes
    ;; boring!
    ;;
    ;; We can't use the `absento` trick to exclude applications with
    ;; this expression representation, since applications aren't
    ;; tagged.
    (absento 'cons expr)
    (occurs-freeo expr (set 'a 'b 'c)))
  '((a (b c))
    ((a b) c)
    ((a b) (c c))
    (((a b) (_.0 c))
     (num _.0))
    (((a b) (_.0 (_.1 c)))
     (num _.0 _.1))
    (((a b) (_.0 (c _.1)))
     (num _.0 _.1))
    (((a b) (c _.0))
     (num _.0))
    (((a b) (c (_.0 _.1)))
     (num _.0 _.1))
    (((a b) (c (_.0 (_.1 _.2))))
     (num _.0 _.1 _.2))
    (((a b) (c (_.0 (lambda (_.1) _.1))))
     (=/= ((_.1 cons)))
     (num _.0)
     (sym _.1))
    (((a b) (c (c _.0)))
     (num _.0))
    (((a b) (c (lambda (_.0) _.0)))
     (=/= ((_.0 cons)))
     (sym _.0))
    (((a b) (c (lambda (_.0) (_.0 _.1))))
     (=/= ((_.0 cons)))
     (num _.1)
     (sym _.0))
    (((a b) (c (lambda (_.0) (_.1 _.0))))
     (=/= ((_.0 cons)))
     (num _.1)
     (sym _.0))
    (((a b) (c ((_.0 _.1) _.2)))
     (num _.0 _.1 _.2))
    (((a b) (c ((lambda (_.0) _.0) _.1)))
     (=/= ((_.0 cons)))
     (num _.1)
     (sym _.0))
    (((a b) (lambda (_.0) (_.0 c)))
     (=/= ((_.0 c)) ((_.0 cons)))
     (sym _.0))
    (((a b) (lambda (_.0) (_.0 (_.1 c))))
     (=/= ((_.0 c)) ((_.0 cons)))
     (num _.1)
     (sym _.0))
    (((a b) (lambda (_.0) (_.0 (c _.1))))
     (=/= ((_.0 c)) ((_.0 cons)))
     (num _.1)
     (sym _.0))
    (((a b) (lambda (_.0) (c _.0)))
     (=/= ((_.0 c)) ((_.0 cons)))
     (sym _.0))))

(test "occurs-freeo-free-play-1"
  (length
   (run 10 (expr free-vars)
     (occurs-freeo expr free-vars)))
  10)

(test "occurs-freeo-free-play-2"
  (length
   (run 10 (expr free-vars)
     (fresh (w)
       ;; ensure at least one variable occurs free
       (ino w free-vars)
       (occurs-freeo expr free-vars))))
  10)

(test "occurs-freeo-free-play-2b"
  ;; run-unique 1000 produces an expression with duplicate args to cons:
  ;; (((cons _.0 _.0) {_.0}) (sym _.0))
  (length
   (run-unique 10 (expr free-vars)
     (fresh (w)
       ;; ensure at least one variable occurs free
       (ino w free-vars)
       (occurs-freeo expr free-vars))))
  9) ;; TODO: should be 10
