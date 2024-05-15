(load "./sets.scm")

(define always-wrap-reified? (make-parameter #f))

; Scope object.
; Used to determine whether a branch has occured between variable
; creation and unification to allow the set-var-val! optimization
; in subst-add. Both variables and substitutions will contain a
; scope. When a substitution flows through a conde it is assigned
; a new scope.

; Creates a new scope that is not scope-eq? to any other scope
(define (new-scope) (list 'scope))
(define scope-eq? eq?)

; Scope used when variable bindings should always be made in the
; substitution, as in disequality solving and reification. We
; don't want to set-var-val! a variable when checking if a
; disequality constraint holds!
(define nonlocal-scope (list 'non-local-scope))


; Logic variable object.
; Contains:
;   val - value for variable assigned by unification using
;      set-var-val! optimization. unbound if not yet set or
;      stored in substitution.
;   scope - scope that the variable was created in.
;   idx - unique numeric index for the variable. Used by the
;      trie substitution representation.
; Variable objects are compared by object identity.

; The unique val for variables that have not yet been bound
; to a value or are bound in the substitution. Also used
; as the sentinal value for substitution and constraint store
; lookup.
(define unbound (list 'unbound))
(define (unbound? v) (eq? v unbound))

(define var
  (let ((counter -1))
    (lambda (scope)
      (set! counter (+ 1 counter))
      (vector unbound scope counter))))

; Vectors are not allowed as terms, so terms that are vectors
; are variables.
(define (var? x) (vector? x))

(define var-eq? eq?)

(define (var-val x)   (vector-ref x 0))
(define (var-scope x) (vector-ref x 1))
(define (var-idx x)   (vector-ref x 2))

(define (set-var-val! x v)
  (vector-set! x 0 v))


; Substitution object.
; Contains:
;   map - mapping of variables to values
;   scope - scope at current program point, for set-var-val!
;     optimization. Updated at conde. Included in the substitution
;     because it is required to fully define the substitution
;     and how it is to be extended.
;
; Implementation of the substitution map depends on the Scheme used,
; as we need a map. See mk.rkt and mk-vicare.scm.

(define empty-subst-map empty-intmap)
(define subst-map-length intmap-count)
(define (subst-map-lookup u S)
  (intmap-ref S (var-idx u)))
(define (subst-map-add S var val)
  (intmap-set S (var-idx var) val))

(define (subst mapping scope)
  (cons mapping scope))

(define subst-map car)
(define subst-scope cdr)

(define (subst-length S)
  (subst-map-length (subst-map S)))

(define (subst-with-scope S new-scope)
  (subst (subst-map S) new-scope))

(define empty-subst (subst empty-subst-map (new-scope)))

(define (subst-add S x v)
  ; set-var-val! optimization: set the value directly on the
  ; variable object if we haven't branched since its creation
  ; (the scope of the variable and the substitution are the same).
  ; Otherwise extend the substitution mapping.
  (if (scope-eq? (var-scope x) (subst-scope S))
    (begin (set-var-val! x v)
           S)
    (subst (subst-map-add (subst-map S) x v) (subst-scope S))))

(define (subst-lookup x S)
  ; set-var-val! optimization.
  ; Tried checking the scope here to avoid a subst-map-lookup
  ; if it was definitely unbound, but that was slower.
  (if (not (unbound? (var-val x)))
    (var-val x)
    (subst-map-lookup x (subst-map S))))

; Association object.
; Describes an association mapping the lhs to the rhs. Returned by
; unification to describe the associations that were added to the
; substitution (whose representation is opaque) and used to represent
; disequality constraints.
(define lhs car)
(define rhs cdr)

; Constraint record object.
;
; Describes the constraints attached to a single variable.
;
; Contains:
;   T - type constraint. instance of type-constraint or #f to indicate
;         no constraint
;   D - list of disequality constraints. Each disequality is a list of
;         associations. The constraint is violated if all associated
;         variables are equal in the substitution simultaneously. D
;         could contain duplicate constraints (created by distinct =/=
;         calls). A given disequality constraint is only attached to
;         one of the variables involved, as all components of the
;         constraint must be violated to cause failure.
;   A - list of absento constraints. Each constraint is a term.
;         The list contains no duplicates.
;   M - list of members. Each constraint is a term.

(define empty-c '(#f () () ()))

(define (c-T c) (car c))
(define (c-D c) (cadr c))
(define (c-A c) (caddr c))
(define (c-M c) (cadddr c))

(define (c-with-T c T) (list T (c-D c) (c-A c) (c-M c)))
(define (c-with-D c D) (list (c-T c) D (c-A c) (c-M c)))
(define (c-with-A c A) (list (c-T c) (c-D c) A (c-M c)))
(define (c-with-M c M) (list (c-T c) (c-D c) (c-A c) M))

; Constraint store object.
; Mapping of representative variable to constraint record. Constraints
; are always on the representative element and must be moved / merged
; when that element changes.

(define empty-C empty-intmap)

(define (set-c st x c)
  (state-with-C
    st
    (intmap-set (state-C st) (var-idx x) c)))

(define (lookup-c st x)
  (let ((res (intmap-ref (state-C st) (var-idx x))))
    (if (unbound? res)
      empty-c
      res)))

; t:unbind in mk-vicare.scm either is buggy or doesn't do what I would expect, so
; I implement remove by setting the value to the empty constraint record.
(define (remove-c x st)
  (state-with-C st (intmap-set (state-C st) (var-idx x) empty-c)))


; State object.
; The state is the value that is monadically passed through the search
; Contains:
;   S - the substitution
;   C - the constraint store

(define (state S C) (cons S C))

(define (state-S st) (car st))
(define (state-C st) (cdr st))

(define empty-state (state empty-subst empty-C))

(define (state-with-C st C^)
  (state (state-S st) C^))

(define state-with-scope
  (lambda (st new-scope)
    (state (subst-with-scope (state-S st) new-scope) (state-C st))))

#|
Given a procedure and a list, for each element of the list,
Calls the procedure with a reversed list of elements to the left,
that element, then the elements to the right.

Ex:

> (map-zipper list '(1 2 3 4 5))

((() 1 (2 3 4 5))
 ((1) 2 (3 4 5))
 ((2 1) 3 (4 5))
 ((3 2 1) 4 (5))
 ((4 3 2 1) 5 ()))
|#
(define (map-zipper proc lst)
  (let loop ([acc   '()]
             [left  '()]
             [right lst])
    (if (null? right)
        (reverse! acc)
        (loop (cons (proc left (car right) (cdr right)) acc)
              (cons (car right) left)
              (cdr right)))))

(define (normalize-set set s)
  (cond
   [(null-set? set)
    set]
   [(nonempty-set? set)
    (make-nonempty-set
     (set-head set)
     (normalize-set (set-tail set) s))]
   [(var? set)
    (let ([walked (walk set s)])
      (if (eq? walked set)
          walked
          (normalize-set walked s)))]
   [else set]))

(define (unify-sets set set^ s)
  (define-values (head  tail)  (set-parts (normalize-set set  s)))
  (define-values (head^ tail^) (set-parts (normalize-set set^ s)))

  ;; When values share a tail, we can induct over one of the sets'
  ;; known elements
  ;; Rule 10 of Dovier et. al
  (define (set-induction)
    ;; Just for shorthand
    (define $ make-nonempty-set)

    (let ([X tail] [t0 (car head)] [t1..m (cdr head)])
      (foldl
       mplus
       #f
       (map-zipper
        (lambda (t^j-1..0 t^j t^j+1..n)
          (let ([head^-t^j (append t^j-1..0 t^j+1..n)]
                [N         (var (car s))]
                [s         (subst-with-scope s (new-scope))])
            (mplus*
             (unify* `([,t0          . ,t^j]
                       [,($ t1..m X) . ,($ head^-t^j X)])
                     s)
             (unify* `([,t0          . ,t^j]
                       [,($ head X)  . ,($ head^-t^j X)])
                     s)
             (unify* `([,t0    . ,t^j]
                       [,($ t1..m X) . ,($ head^ X)])
                     s)
             ;; TODO: this normally includes a set type-constraint on N
             ;; I'm leaving it out right now, but it needs to be added later
             (unify* `([,tail^       . ,(set-cons t0 N)]
                       [,($ t1..m N) . ,($ head^ N)])
                     s))))
        head^))))

  ;; When values don't share a tail, we have to enumerate all possible
  ;; set combinations
  ;; Rule 9 of Dovier et. al
  (define (set-enumeration)
    (let ([t  (set-first set)]
          [r  (set-rest  set)]
          [t^ (set-first set^)]
          [r^ (set-rest  set^)]
          [N  (var (car s))]
          [s  (subst-with-scope s (new-scope))])
      (mplus*
       (unify* `([,t . ,t^]
                 [,r . ,r^])
               s)
       (unify* `([,t   . ,t^]
                 [,set . ,r^])
               s)
       (unify* `([,t . ,t^]
                 [,r . ,set^])
               s)
       ;; TODO: this normally includes a set type-constraint on N
       ;; I'm leaving it out right now, but it needs to be added later
       (unify* `([,r  . ,(set-cons t^ N)]
                 [,r^ . ,(set-cons t N)])
               s))))

  (if (eq? tail tail^)
      (set-induction)
      (set-enumeration)))
  
; Unification

; UnificationResult: (Streamof (Pair Substitution (Listof Association)))
; A stream of extended substitutions and lists of associations added during the unification,
;  or the empty stream, indicating unification failed.
; This may have any number of elements, but should be finite.

; Term, Term, Substitution -> UnificationResult
(define (unify u v s)
  (let ((u (walk u s))
        (v (walk v s)))
    (cond
      ((eq? u v) (cons s '()))
      ((and (var? u) (var? v))
       (if (> (var-idx u) (var-idx v))
         (ext-s-check u v s)
         (ext-s-check v u s)))
      ((var? u) (ext-s-check u v s))
      ((var? v) (ext-s-check v u s))
      ((and (set-pair? u) (set-pair? v)) (unify-sets u v s))
      ((and (pair? u) (pair? v))
       (let*-bind ([s.added-car (unify (car u) (car v) s)]
                   [s.added-cdr (unify (cdr u) (cdr v) (car s.added-car))])
         (cons (car s.added-cdr) (append (cdr s.added-car) (cdr s.added-cdr)))))
      ((equal? u v) (cons s '()))
      (else #f))))

; Term, Substitution -> Term
(define (walk u S)
  (let rec ((u u))
    (if (var? u)
      (let ((val (subst-lookup u S)))
        (if (unbound? val)
          u
          (rec val)))
      u)))

; Var, Term, Substitution -> Boolean
(define (occurs-check x v S)
  (let ((v (walk v S)))
    (cond
      ((var? v) (var-eq? v x))
      ((set-pair? v)
       (or (occurs-check x (set-first v) S)
           (occurs-check x (set-rest v) S)))
      ((pair? v)
       (or (occurs-check x (car v) S)
           (occurs-check x (cdr v) S)))
      (else #f))))

; Var, Term, Substitution -> UnificationResult
(define (ext-s-check x v S)
  (if (occurs-check x v S)
      #f
      (cons (subst-add S x v) (list (cons x v)))))

; Term, Term, Substitution -> UnificationResult
(define (unify* S+ S)
  (unify (map lhs S+) (map rhs S+) S))

; Search

; For some type a ∌ #f, (Streamof b), (Pair b Procedure)
; (Streamof a): #f | (SuspendedStreamof a) | a | (Pair a (SuspendedStreamof a))
; (SuspendedStreamof a): (-> (Streamof a))
; SearchStream: (Streamof State)
; SuspendedStream: (SuspendedStreamof State)
; (Expansion a): (a -> (Streamof a))
; Goal: (Expansion State)

; Match on search streams. The State type must not be a pair with a procedure
; in its cdr, lest a single result be interpreted as multiple results.
;
; (() e0)     failure
; ((f) e1)    suspension for interleaving. separate from success or failure to ensure
;              it goes all the way to the top of the tree.
; ((c) e2)    single result. Used rather than (cons c (lambda () #f))
;              to avoid returning to search a part of the tree that
;              will inevitably fail.
; ((c f) e3)  multiple results. `f` is a thunk to avoid unnecessary computation
;              in the case that the LHS the last answer needed to satisfy the
;              query. It also triggers interleaving; the search looks for
;              answers in alternate branches before returning.
(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((c^) e2) ((c f) e3))
     (let ((stream e))
       (cond
         ((not stream) e0)
         ((procedure? stream)  (let ((f^ stream)) e1))
         ((not (and (pair? stream)
                 (procedure? (cdr stream))))
          (let ((c^ stream)) e2))
         (else (let ((c (car stream)) (f (cdr stream)))
                 e3)))))))

; (Streamof a), (SuspendedStreamof a) -> (SearchStream a),
;
; f is a thunk to avoid unnecesarry computation in the case that the
; first answer produced by c-inf is enough to satisfy the query.
(define (mplus stream f^)
  (define f (if (procedure? f^) f^ (suspend f^)))
  (case-inf stream
    (() (f))
    ((f^) (lambda () (mplus (f) f^)))
    ((c) (cons c f))
    ((c f^) (cons c (lambda () (mplus (f) f^))))))

; (Streamof a), (Expander a) -> (Streamof a)
(define (bind stream g)
  (case-inf stream
    (() #f)
    ((f) (lambda () (bind (f) g)))
    ((c) (g c))
    ((c f) (mplus (g c) (lambda () (bind (f) g))))))

; Int, (Streamof a) -> (Listof a)
(define (take n f)
  (if (and n (zero? n))
      '()
      (case-inf f
        (() '())
        ((f) (take n (f)))
        ((c) (cons c '()))
        ((c f) (cons c (take (and n (- n 1)) (f)))))))

(define (list->stream lst)
  (cond
   [(null? lst) #f]
   [(pair? lst) (let ([rest (list->stream (cdr lst))])
                  (cons (car lst) (suspend rest)))]))

(define (stream . xs)
  (list->stream xs))

#|
A macro for writing code with multiple `bind` calls akin to `do` notation in haskell.
Each LHS of the binders is bound to each of the values on the stream produced by the RHS.
The scope of each RHS has access to prior binders, à la let*
|#
(define-syntax let*-bind
  (syntax-rules ()
    [(let*-bind ()
       body body* ...)
     (let ()
       body body* ...)]
    [(let*-bind ([first-name first-value] more-clauses ...)
       body body* ...)
     (bind
      first-value
      (lambda (first-name)
        (let*-bind (more-clauses ...)
          body body* ...)))]))

; (bind* e:(Streamof a) g:(Expander a) ...) -> (Streamof a)
(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

; (suspend e:(Streamof a)) -> (SuspendedStreamof a)
; Used to clearly mark the locations where search is suspended in order to
; interleave with other branches.
(define-syntax suspend (syntax-rules () ((_ body) (lambda () body))))

; (mplus* e:(Streamof a) ...+) -> (Streamof a)
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...)
     (mplus e0 (suspend (mplus* e ...))))))

; (fresh (x:id ...) g:Goal ...+) -> Goal
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambda (st)
       (suspend
         (let ((scope (subst-scope (state-S st))))
           (let ((x (var scope)) ...)
             (bind* (g0 st) g ...))))))))

; (conde [g:Goal ...] ...+) -> Goal
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambda (st)
       (suspend
         (let ((st (state-with-scope st (new-scope))))
           (mplus*
             (bind* (g0 st) g ...)
             (bind* (g1 st) g^ ...) ...)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (take n
           (suspend
             ((fresh (q) g0 g ...
                     (lambda (st)
                       (let ((st (state-with-scope st nonlocal-scope)))
                         (let ((z ((reify q) st)))
                           (cons z (lambda () (lambda () #f)))))))
              empty-state))))
    ((_ n (q0 q1 q ...) g0 g ...)
     (run n (x)
       (fresh (q0 q1 q ...)
         g0 g ...
         (== (list q0 q1 q ...) x))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))


(define-syntax defrel
  (syntax-rules ()
   ;; No suspend given single goal; search order for relations designed with define
   ((_ (name arg ...) g)
    (define (name arg ...) g))
   ;; Use of fresh suspends when forming a conjunction to avoid nontermination
   ((_ (name arg ...) g ...)
    (define (name arg ...) (fresh () g ...)))))



; Constraints
; C refers to the constraint store map
; c refers to an individual constraint record

; SimpleConstraint: State -> (or/c #f State)
;
; (note that a SimpleConstraint is a Goal but a Goal is not a SimpleConstraint.
;  SimpleConstraint implementations currently use this more restrained type.
;  See `and-foldl` and `update-constraints`.)

; Invariants assumed for type constraints:
; 1. Must be positive, not negative. not-pairo wouldn't work.
; 2. Each type must have infinitely many possible values to avoid
;      incorrectness in combination with disequality constraints,
;      like: (fresh (x) (booleano x) (=/= x #t) (=/= x #f))
; 3. Types must be disjoint from each other and from pairs.

; Predicate: Any -> (or #f Any)
; CompareResult: (or '< '> '=)
; Ordering: T, T -> CompareResult where T is defined by the corresponding predicate.
; Propagator: T -> SimpleConstraint

; Predicate Symbol CompareResult (or/c #f Propagator) -> TypeConstraint
(define type-constraint
  (case-lambda
    [(predicate reified ordering)
     (list predicate reified ordering atomic-propagator)]
    [(predicate reified ordering propagator)
     (list predicate reified ordering propagator)]))

;; Propagator for atomic values: always succeeds as there is no propagation
(define (atomic-propagator new-obj)
  succeed)

(define type-constraint-predicate car)
(define type-constraint-reified cadr)
(define type-constraint-ordering caddr)
(define type-constraint-propagator cadddr)

; TypeConstraint -> (Term -> Goal)
(define (apply-type-constraint tc)
  (lambda (u)
    (lambda (st)
      (let ((type-pred (type-constraint-predicate tc))
            (type-prop (type-constraint-propagator tc)))
        (let ((term (walk u (state-S st))))
          (cond
            ;; ((type-pred term) st)
            ((type-pred term)
             ((type-prop term) st))
            ((var? term)
             (let* ((c (lookup-c st term))
                    (T (c-T c)))
               (cond
                 ((eq? T tc) st)
                 ((not T) (set-c st term (c-with-T c tc)))
                 (else #f))))
            (else #f)))))))

(define-syntax declare-type-constraints
  (syntax-rules ()
    ((_ tc-list (name predicate reified ordering more ...) ...)
     (begin
       (define tc-list
         (list (type-constraint predicate 'reified ordering more ...) ...))
       (define-values
         (name ...)
         (apply values (map apply-type-constraint tc-list)))))))

; Orderings
(define (number-compare a b) (cond ((= a b) '=) ((< a b) '<) (else '>)))
(define (string-compare a b) (cond ((string=? a b) '=) ((string<? a b) '<) (else '>)))
(define (symbol-compare a b) (string-compare (symbol->string a) (symbol->string b)))

(define (set-compare x y)
  (cond
   [(and (set-null? x) (set-null? y)) '=]
   [(and (set-null? x) (set-pair? y)) '<]
   [(and (set-pair? x) (set-null? y)) '>]
   [(and (set-pair? x) (set-pair? y))
    (let ((r (lex-compare (set-first x) (set-first y))))
      (if (eq? r '=)
          (lex-compare (set-rest x) (set-rest y))
          r))]))

(define (valid-seto x)
  (if (null-set? x)
      succeed
      (seto (set-rest x))))

(declare-type-constraints type-constraints
  (numbero number? num number-compare)
  (stringo string? str string-compare)
  (symbolo symbol? sym symbol-compare)
  (seto    set?    set set-compare    valid-seto))

(define (add-to-D st v d)
  (let* ((c (lookup-c st v))
         (c^ (c-with-D c (cons d (c-D c)))))
    (set-c st v c^)))

; (Listof Association) -> Goal
(define (=/=* S+)
  (lambda (st)
    (let ((S.added-inf (unify* S+ (subst-with-scope (state-S st) nonlocal-scope))))
      (fold-inf
       (lambda (st S.added)
         (and st
              (let ((added (cdr S.added)))
                (and (pair? added)
                     (let* ([el (car added)]
                            [st (add-to-D st (car el) added)])
                       (if (var? (cdr el))
                           (add-to-D st (cdr el) added)
                           st))))))
       st
       S.added-inf))))

; Term, Term -> Goal
(define (=/= u v)
  (=/=* (list (cons u v))))

;; State, Var, Term -> State
;; Add the constraint that a variable must unify to a term to a state
(define (add-to-M st v trm)
  (let* ((c  (lookup-c st v))
         (c^ (c-with-M c (cons trm (c-M c)))))
    (set-c st v c^)))

;; Term, Term -> Goal
;; Holds when `i` is a member of `s` 
(define (ino i s)
  (project (s)
    (cond
     [(set-pair? s) (conde
                     ;; possible duplicate introduced here
                     [(== i (set-first s))]
                     [(ino i (set-rest s))])]
     [(var? s)      (fresh (k)
                      (seto k)
                      (== s (set-cons i k)))]
     [else          fail])))

; Term, Term -> Goal
; Generalized 'absento': 'term1' can be any legal term (old version
; of faster-miniKanren required 'term1' to be a ground atom).
(define (absento term1 term2)
  (lambda (st)
    (let ((term1 (walk term1 (state-S st)))
          (term2 (walk term2 (state-S st))))
      (let ((st^ ((=/= term1 term2) st)))
        (and st^
             (cond
              ((pair? term2)
               (let ((st^^ ((absento term1 (car term2)) st^)))
                 (and st^^ ((absento term1 (cdr term2)) st^^))))
              ((set-pair? term2)
               (let ((st^^ ((absento term1 (set-first term2)) st^)))
                 (and st^^ ((absento term1 (set-rest term2)) st^^))))
              ((var? term2)
               (let* ((c (lookup-c st^ term2))
                      (A (c-A c)))
                 (if (memv term1 A)
                   st^
                   (let ((c^ (c-with-A c (cons term1 A))))
                     (set-c st^ term2 c^)))))
              (else st^)))))))

; (State, Any -> SimpleConstraint), State, List -> SimpleConstraint
; Fold lst with proc and initial value init. If proc ever returns #f,
; return with #f immediately. Used for applying a series of
; constraints to a state, failing if any operation fails.
(define (and-foldl proc init lst)
  (if (null? lst)
    init
    (let ((res (proc (car lst) init)))
      (and res (and-foldl proc res (cdr lst))))))

;; (Listof (Expander a)) -> (Expander a)
;; A goal constructor which takes a list of goals, conjoining them in series.
;; `(then ...)` is equivalent to `(fresh () ...)`, but is a procedure instead of syntax
;; Equivalent to a variadic `>=>` operator
(define (then . goals)
  (define (then2 goal1 goal2)
    (lambda (st)
      (bind (goal1 st) goal2)))
  (foldl then2 succeed goals))

;; a, (Listof Goal) -> SearchStream
;; Applies a list of goals to an initial state
;; `(bind-foldl state (list goal ...))` is equivalent to `(bind* state goal ...)`,
;; but is a procedure, rather than syntax
(define (bind-foldl state goals)
  ((apply then goals) state))

;; (acc elem -> acc) acc (Streamof elem) -> acc
;; Folds the accumulator over a stream of values
;; This forces the stream, so it must be finite
(define (fold-inf proc init strm)
  (case-inf strm
    [()    init]
    [(f)   (fold-inf poc init (f))]
    [(c)   (proc init c)]
    [(c f) (let ((init^ (proc init c)))
             (fold-inf proc init^ (f)))]))

;; (in -> out) -> Streamof in -> Streamof out
;; Maps the stream over a given procedure
(define (map-inf proc strm)
  (case-inf strm
    [()    #f]
    [(f)   (suspend (map-inf proc (f)))]
    [(c)   (proc c)]
    [(c f) (cons (proc c) (suspend (map-inf proc (f))))]))

(define (== u v)
  (lambda (st)
    (let*-bind ([S^.added (unify u v (state-S st))])
      (let* ([S^    (car S^.added)]
             [added (cdr S^.added)]
             [st^ (state S^ (state-C st))])
        (bind-foldl st^ (map update-constraints added))))))

;; Association -> Goal
;; Given a new association, return a goal which is its logical consequence,
;; WRT the given constraint
(define (update-constraints a)
  ;; Not fully optimized. Could do absento update with fewer
  ;; hash-refs / hash-sets.
  (lambda (st)
    (let ((old-c (lookup-c st (lhs a))))
      (if (eq? old-c empty-c)
        st
        (let ((st (remove-c (lhs a) st)))
          (bind-foldl st
                      (append
                        (if (c-T old-c)
                            (list ((apply-type-constraint (c-T old-c)) (rhs a)))
                            '())
                        (map (lambda (atom) (absento atom (rhs a))) (c-A old-c))
                        (map =/=* (c-D old-c))
                        (map (lambda (mem) (ino mem (rhs a))) (c-M old-c))))))))) 

(define (walk* v S)
  (let ((v (walk v S)))
    (cond
      ((var? v) v)
      ((set-pair? v)
       (set-cons (walk* (set-first v) S) (walk* (set-rest v) S)))
      ((pair? v)
       (cons (walk* (car v) S) (walk* (cdr v) S)))
      (else v))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambda (st)
       (let ((x (walk* x (state-S st))) ...)
         ((fresh () g g* ...) st))))))

(define succeed (lambda (x) x))
(define fail (lambda _ #f))

; Reification

; S - substitution
; T - type constraints
; A - absento constriants
; D - disequality constraints
; M - membership constraints

(define (reify x)
  (lambda (st)
    (let* ((S (state-S st))
           (v (walk* x S))
           (R (reify-S v (subst empty-subst-map nonlocal-scope)))
           (v (simplify-S v R))
           (relevant-vars (vars v)))
      (let*-values (((T D A M) (extract-and-normalize st relevant-vars x))
                    ((D A)   (drop-irrelevant D A relevant-vars))
                    ((D A)   (drop-subsumed D A st)))
        (form (walk* v R)
              (walk* D R)
              (walk* T R)
              (walk* A R)
              (walk* M R))))))

(define (vars term)
  (let rec ((term term) (acc '()))
    (cond
      ((var? term) (cons term acc))
      ((set-pair? term)
       (rec (set-first term) (rec (set-rest term) acc)))
      ((pair? term)
       (rec (cdr term) (rec (car term) acc)))
      (else (remove-duplicates acc)))))

(define (extract-and-normalize st relevant-vars x)
  (define T (map (lambda (tc-type)
                   (cons (type-constraint-reified tc-type)
                         (filter-map (lambda (x)
                                       (let ((tc (c-T (lookup-c st x))))
                                         (and (eq? tc tc-type)
                                              x)))
                                     relevant-vars)))
                 type-constraints))
  (define D (append*
              (map (lambda (x)
                     (filter-map (normalize-diseq (state-S st)) (c-D (lookup-c st x))))
                   relevant-vars)))
  (define A (append*
              (map (lambda (x)
                     (map (lambda (a-lhs)
                            (cons (walk* a-lhs (state-S st))
                                  x))
                          (c-A (lookup-c st x))))
                   relevant-vars)))
  (define M (append*
             (map (lambda (x)
                    (map (lambda (m-lhs)
                           (cons (walk* m-lhs (state-S st))
                                 x))
                         (c-M (lookup-c st x))))
                  relevant-vars)))
  (values T D A M))

(define (normalize-diseq S)
  ;; FIXME:
  ;; The `error` calls here are from generalizing UnificationResult
  ;; I'm not 100% sure how this procedure should be generalized,
  ;; so I'm adding errors in the new cases.
  (lambda (S+)
    (case-inf (unify* S+ S)
      [()    #f]
      [(f)   (error 'normalize-diseq "Normalizing unification is suspended")]
      [(c)   (walk* (cdr c) S)]
      [(c f) (error 'normalize-diseq "Normalizing unification is divergent")])))

; Drop constraints that are satisfiable in any assignment of the reified
; variables, because they refer to unassigned variables that are not part of
; the answer, which can be assigned as needed to satisfy the constraint.
(define (drop-irrelevant D A relevant-vars)
  (define (all-relevant? t)
    (andmap (lambda (v) (member v relevant-vars))
            (vars t)))
  (values (filter all-relevant? D)
          (filter all-relevant? A)))


(define (drop-subsumed D A st)
  (define D^ (rem-subsumed
                 d-subsumed-by?
                 (filter (lambda (d)
                           (not (or (d-subsumed-by-T? d st)
                                    (d-subsumed-by-A? d A st))))
                         D)))
  (define A^ (rem-subsumed
                 a-subsumed-by?
                 (filter (lambda (a)
                           (not (or (absento-rhs-atomic? a st)
                                    (absento-rhs-occurs-lhs? a st))))
                         A)))
  (values D^ A^))

; Drop absento constraints where the RHS is known to be atomic, such that
; the disequality attached by absento solving is sufficient.
(define (absento-rhs-atomic? a st)
  ; absento on pairs is pushed down and type constraints are atomic,
  ; so the only kind of non-atomic RHS is an untyped var.
  (not (and (var? (rhs a)) (unbound? (var-type (rhs a) st)))))

; Drop absento constraints that are trivially satisfied because
; any violation would cause a failure of the occurs check.
; Example:
;  (absento (list x y z) x) is trivially true because a violation would
;  require x to occur within itself.
(define (absento-rhs-occurs-lhs? a st)
  (occurs-check (rhs a) (lhs a) (state-S st)))

; Drop disequalities that are subsumed by an absento contraint
; that is not itself equivalent to just a disequality.
(define (d-subsumed-by-A? d A st)
  (exists (lambda (a)
            (and (not (absento-rhs-atomic? a st))
                 (d-subsumed-by? d (absento->diseq a))))
          A))

; Drop disequalities that are fully satisfied because the types are disjoint
; either due to type constraints or ground values.
; Examples:
;  * given (symbolo x) and (numbero y), (=/= x y) is dropped.
(define (d-subsumed-by-T? d st)
  (exists (lambda (pr) (not (var-types-match? (lhs pr) (rhs pr) st)))
          d))

(define (var-types-match? t1 t2 st)
  (or (unbound? (var-type t1 st))
      (if (var? t2)
        (or (unbound? (var-type t2 st))
            (eq? (var-type t1 st) (var-type t2 st)))
        ((type-constraint-predicate (var-type t1 st))
         t2))))

(define (var-type x st) (or (c-T (lookup-c st x)) unbound))

(define (absento->diseq t) (list t))

(define (rem-subsumed subsumed-by? el*)
  (define (subsumed-by-one-of? el el*)
    (ormap (lambda (el2) (subsumed-by? el el2)) el*))

  (let loop ((el* el*)
             (result '()))
    (cond
      ((null? el*) result)
      (else
        (let ((el (car el*)))
          (cond
            ((or (subsumed-by-one-of? el (cdr el*))
                 (subsumed-by-one-of? el result))
             (loop (cdr el*) result))
            (else (loop (cdr el*)
                        (cons el result)))))))))

; Examples:
; * (absento `(cat . ,S) y) is subsumed by (absento S y)
;
; Note that absento constraints are pushed down to tree leaves, so we would never have
;  (absento 'closure q) given (== q (list x)). Thus we do not need to consider subsumption
;  between absento constraints on q and x.
(define (a-subsumed-by? t1 t2)
  (and (var-eq? (rhs t1) (rhs t2)) (member* (lhs t2) (lhs t1))))

(define (member* u v)
  (cond
    ((equal? u v) #t)
    ((pair? v)
     (or (member* u (car v)) (member* u (cdr v))))
    (else #f)))

; (-> disequality/c disequality/c boolean?)
; Examples:
;  * ((a . 5) (b . 6)) is subsumed by ((a . 5)) because (not (== a 5)) is a stronger constraint
;    than (not (and (== a 5) (== b 6)))
(define (d-subsumed-by? d1 d2)
  ;; All the different ways d1 can possibly unify
  (define unifications (map-inf car (unify* d1 (subst empty-subst-map nonlocal-scope))))

  (fold-inf
   (lambda (acc elem) (or acc elem))
   #f
   (let*-bind ((S        unifications)
               (S+.added (unify* d2 S)))
     (null? (cdr S+.added)))))

(define (reify-S v S)
  (let ((v (walk v S)))
    (cond
      ((var? v)
       (let ((n (subst-length S)))
         (let ((name (reify-name n)))
           (subst-add S v name))))
      ((set-pair? v)
       (let ((S (reify-S (set-first v) S)))
         (reify-S (set-rest v) S)))
      ((pair? v)
       (let ((S (reify-S (car v) S)))
         (reify-S (cdr v) S)))
      (else S))))

; Formatting

(define (reify-name n)
  (string->symbol
    (string-append "_" "." (number->string n))))

(define (form v D T A M)
  (let ((ft (filter-map
              (lambda (p)
                (let ((tc-type (car p)) (tc-vars (cdr p)))
                  (and (not (null? tc-vars))
                       `(,tc-type . ,(sort-lex tc-vars)))))
              T))
        (fd (sort-D D))
        (fa (sort-lex A))
        (fm (sort-lex M)))
    (let ((fd (if (null? fd) fd
                (let ((fd (drop-dot-D fd)))
                  `((=/= . ,fd)))))
          (fa (if (null? fa) fa
                (let ((fa (drop-dot fa)))
                  `((absento . ,fa)))))
          (fm (if (null? fm)
                  fm
                  `((in ,(cdar fm) ,@(map car fm))))))
      (cond
        ((and (null? fd) (null? ft) (null? fa) (null? fm) (not (always-wrap-reified?)))
         v)
        (else (append `(,v) fd ft fa fm))))))

(define (sort-lex ls)
  (list-sort lex<=? ls))

(define (sort-D D)
  (sort-lex (map sort-d D)))

(define (sort-d d)
  (list-sort lex<=? (map sort-pr d)))

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define (sort-pr pr)
  (let ((l (lhs pr)) (r (rhs pr)))
    (cond
      ((not (reified-var? r)) pr)
      ((symbol<? r l) `(,r . ,l))
      (else pr))))

(define (reified-var? r)
  (and (symbol? r)
       (char=? (string-ref (symbol->string r) 0)
               #\_)))

(define (drop-dot-D D)
  (map drop-dot D))

(define (drop-dot X)
  (map (lambda (t) (list (lhs t) (rhs t)))
       X))

; (Listof (Pair Predicate Ordering))
(define type-orderings
  (append
    ; atomic types
    (map (lambda (tc) (cons (type-constraint-predicate tc)
                            (type-constraint-ordering tc)))
         type-constraints)
    `(; booleans
      (,(lambda (v) (eq? v #f)) . ,(lambda (x y) '=))
      (,(lambda (v) (eq? v #t)) . ,(lambda (x y) '=))
      ; lists
      (,null? . ,(lambda (x y) '=))
      (,pair? . ,(lambda (x y)
                   (let ((r (lex-compare (car x) (car y))))
                     (if (eq? r '=)
                       (lex-compare (cdr x) (cdr y))
                       r))))
      ; sets
      (,set-null? . ,(lambda (x y) '=))
      (,set-pair?  . ,(lambda (x y)
                        (let ((r (lex-compare (set-first x) (set-first y))))
                          (if (eq? r '=)
                              (lex-compare (set-rest x) (set-rest y))
                              r)))))))

(define (index+element-where l pred)
  (let loop ((l l)
             (i 0))
    (cond
      [(null? l)      (values #f #f)]
      [(pred (car l)) (values i (car l))]
      [else           (loop (cdr l) (+ i 1))])))

(define (type-ordering v)
  (let-values ([(idx pr) (index+element-where type-orderings (lambda (pr) ((lhs pr) v)))])
    (if idx
      (values idx (rhs pr))
      (error 'type-index "missing ordering for type of value ~s" v))))

; (Term, Term) -> (or CompareResult error)
; defined when arguments are pairs, null, or atomic types addressed by type-constraints;
; see type-orderings.
(define (lex-compare x y)
  (let-values (((x-o-idx x-o) (type-ordering x))
               ((y-o-idx y-o) (type-ordering y)))
    (if (eqv? x-o-idx y-o-idx)
      (x-o x y)
      (number-compare x-o-idx y-o-idx))))

(define (lex<=? x y)
  (member (lex-compare x y) '(< =)))

(define (unique elms)
  (sort-lex (remove-duplicates elms)))

(define (simplify-S obj sub)
  (cond
   [(pair? obj)
    (cons (simplify-S (car obj) sub)
          (simplify-S (cdr obj) sub))]
   [(set-pair? obj)
    (let* ([s (normalize-set obj sub)]
           [h (unique (map (lambda (x) (simplify-S x sub)) (set-head s)))]
           [t (set-tail s)])
      (make-nonempty-set h t))]
   [else obj]))

