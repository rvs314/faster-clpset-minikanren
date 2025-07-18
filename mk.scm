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

(define *var-counter* (make-parameter -1))

(define var
  (case-lambda
    [(scope)
     (var scope 'unnamed)]
    [(scope name)
     (*var-counter* (+ 1 (*var-counter*)))
     (vector 'var unbound scope (*var-counter*) name)]))

;; Variables are represented as vectors tagged with an initial
;; symbol `'var`
(define (var? x)
  (and (vector? x)
       (eq? 'var (vector-ref x 0))))

(define var-eq? eq?)

(define (var-val x)   (vector-ref x 1))
(define (var-scope x) (vector-ref x 2))
(define (var-idx x)   (vector-ref x 3))
(define (var-name x)  (vector-ref x 4))

(define (set-var-val! x v)
  (vector-set! x 1 v))


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
  (if (and (scope-eq? (var-scope x) (subst-scope S)) (not (scope-eq? (var-scope x) nonlocal-scope)))
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
;   B - list of sub-absento constraints. Each constraint is a term.
;         The list contains no duplicates.
;   M - list of non-members. Each constraint is a term.
;   E - list of variables which are mutually exclusive with the
;       associated variable
;   U - list of three-element lists. Each (list X Y Z) is
;       the constraint that X ∪ Y = Z. The associated variable should
;       be at least one of X, Y or Z
;   F - list of non-keys for an alist or #f.
;       If #f, is a noop.
;       If a list, then it implies `listo`, and ensures that
;       the list is an association list (list of pairs)
;       and that no element of the constraint appears on the lhs of any pair
;   L - list of associations in an alist or #f.
;       If #f, is a noop.
;       If it is a list, it implies `listo`, and ensures that
;       the list is an association list (list of pairs)
;       and that the association list maps the given association

(define empty-c '(#f () () () () () #f #f))

(define (c-T c) (car c))
(define (c-D c) (cadr c))
(define (c-B c) (caddr c))
(define (c-M c) (cadddr c))
(define (c-E c) (car (cddddr c)))
(define (c-U c) (cadr (cddddr c)))
(define (c-F c) (caddr (cddddr c)))
(define (c-L c) (cadddr (cddddr c)))

(define (c-with-T c T) (list T (c-D c) (c-B c) (c-M c) (c-E c) (c-U c) (c-F c) (c-L c)))
(define (c-with-D c D) (list (c-T c) D (c-B c) (c-M c) (c-E c) (c-U c) (c-F c) (c-L c)))
(define (c-with-B c B) (list (c-T c) (c-D c) B (c-M c) (c-E c) (c-U c) (c-F c) (c-L c)))
(define (c-with-M c M) (list (c-T c) (c-D c) (c-B c) M (c-E c) (c-U c) (c-F c) (c-L c)))
(define (c-with-E c E) (list (c-T c) (c-D c) (c-B c) (c-M c) E (c-U c) (c-F c) (c-L c)))
(define (c-with-U c U) (list (c-T c) (c-D c) (c-B c) (c-M c) (c-E c) U (c-F c) (c-L c)))
(define (c-with-F c F) (list (c-T c) (c-D c) (c-B c) (c-M c) (c-E c) (c-U c) F (c-L c)))
(define (c-with-L c L) (list (c-T c) (c-D c) (c-B c) (c-M c) (c-E c) (c-U c) (c-F c) L))

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

(define (state-with-S st S^)
  (state S^ (state-C st)))

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

;; Converts from two-element lists to pairs
(define (list->pair lst)
  (assert (= (length lst) 2))
  (cons (car lst) (cadr lst)))

;; Converts from pairs to two-element lists
(define (pair->list pr)
  (assert (pair? pr))
  (list (car pr) (cdr pr)))

(define (normalize-set set s)
  (cond
   [(null-set? set)
    '#(set)]
   [(set? set)
    (make-set
     (remove-duplicates (set-head set))
     (normalize-set (set-tail set) s))]
   [(var? set)
    (let ([walked (walk set s)])
      (if (eq? walked set)
          walked
          (normalize-set walked s)))]
   [else set]))

(define-syntax suspend (syntax-rules () ((_ body) (lambda () body))))

; (mplus* e:(Streamof a) ...+) -> (Streamof a)
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...)
     (mplus e0 (suspend (mplus* e ...))))))

; (bind* e:(Streamof a) g:(Expander a) ...) -> (Streamof a)
(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))

; UnificationResult: (Streamof (Pair State (Listof Association)))
; A stream of extended substitutions and lists of associations added during the unification,
;  or the empty stream, indicating unification failed.
; This may have any number of elements, but should be finite.

; Term, Term, Substitution -> UnificationResult

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

; (fresh (x:id ...) g:Goal ...+) -> Goal
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambda (st)
       (suspend
         (let ((scope (subst-scope (state-S st))))
           ;; Use let* to force left-to-right variable allocation order
           (let* ((x (var scope 'x)) ...)
             ;; Use let to get a duplicate binding error.
             (let ((x x) ...)
               (bind* (g0 st) g ...)))))))))

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

(define disj
  (case-lambda
    [()       fail]
    [(k)      k]
    [(k j)    (conde [k] [j])]
    [(k . ks) (fold-left disj k ks)]))

(define conj
  (case-lambda
    [()       succeed]
    [(k)      k]
    [(k j)    (lambda (st) (bind* st k j))]
    [(k . ks) (fold-left conj k ks)]))

(define-syntax log
  (syntax-rules (:no-reifier)
    [(log :no-reifier expr ...)
     (lambda (st)
       (display (list (list 'expr '=> expr) ...))
       (newline)
       (succeed st))]
    [(log expr ...)
     (lambda (st)
       (display ((reify (list (list 'expr '=> expr) ...)) st))
       (newline)
       (succeed st))]))

(define-syntax toplevel-query
  (syntax-rules ()
    [(toplevel-query (q) g0 g ...)
     (suspend
       ((fresh (q) g0 g ...
               (lambda (st)
                 (let ((st (state-with-scope st nonlocal-scope)))
                   (let ((z ((reify q) st)))
                     (cons z (lambda () (lambda () #f)))))))
        empty-state))]))

(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (take n (toplevel-query (q) g0 g ...)))
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

(define (unify-sets set set^ st)
  (define s (state-S st))
  (define-values (head  tail)  (set-parts (normalize-set set  s)))
  (define-values (head^ tail^) (set-parts (normalize-set set^ s)))

  ;; When values share a tail, we can induct over one of the sets'
  ;; known elements
  ;; Rule 10 of Dovier et. al
  (define (set-induction)
    ;; Just for shorthand
    (define $ make-set)

    (let ([X tail] [t0 (car head)] [t1..m (cdr head)])
      (foldl
       mplus
       #f
       (map-zipper
        (lambda (t^j-1..0 t^j t^j+1..n)
          (let* ([head^-t^j (append t^j-1..0 t^j+1..n)]
                 [st        (state-with-scope st (new-scope))])
            (mplus*
             (unify* `([,t0          . ,t^j]
                       [,($ t1..m X) . ,($ head^-t^j X)])
                     st)
             (unify* `([,t0          . ,t^j]
                       [,($ head X)  . ,($ head^-t^j X)])
                     st)
             (unify* `([,t0          . ,t^j]
                       [,($ t1..m X) . ,($ head^ X)])
                     st)
             (let* ([N         (var (car s) 'N)]
                    [st        ((seto N) st)])
               (unify* `([,tail^       . ,($ (list t0) N)]
                         [,($ t1..m N) . ,($ head^ N)])
                       st)))))
        head^))))

  ;; When values don't share a tail, we have to enumerate all possible
  ;; set combinations
  ;; Rule 9 of Dovier et. al
  (define (set-enumeration)
    (let* ([t  (set-first set)]
           [r  (set-rest  set)]
           [t^ (set-first set^)]
           [r^ (set-rest  set^)]
           [st (state-with-scope st (new-scope))])
      (mplus*
       (unify* `([,t . ,t^]
                 [,r . ,r^])
               st)
       (unify* `([,t   . ,t^]
                 [,(set* t r) . ,r^])
               st)
       (unify* `([,t . ,t^]
                 [,r . ,set^])
               st)
       (let* ([M   (var (subst-scope (state-S st)) 'M)]
              [st^ ((seto M) st)])
         (unify* `([,r  . ,(set-cons t^ M)]
                   [,r^ . ,(set-cons t M)])
                 st^)))))

  (if (equal? tail tail^)
      (set-induction)
      (set-enumeration)))

; Unification

(define (unify u v st)
  (define s (state-S st))

  (define (extend u v)
    (map-inf
     (lambda (S.added)
       (cons
        (state-with-S st (car S.added))
        (cdr S.added)))
     (ext-s-check u v s)))

  (define (chasing-tail u v)
    (let ((new-var (var nonlocal-scope)))
      (unify (make-set (set-head u) new-var)
             v
             st)))

  (let ((u (normalize-set (walk u s) s))
        (v (normalize-set (walk v s) s)))
    (cond
      ((eq? u v) (cons st '()))
      ((and (var? u) (var? v))
       (if (> (var-idx u) (var-idx v))
         (extend u v)
         (extend v u)))
      ((and (set-pair? u) (var? v) (var-eq? v (set-tail u))) (chasing-tail u v))
      ((and (set-pair? v) (var? u) (var-eq? u (set-tail v))) (chasing-tail v u))
      ((var? u) (extend u v))
      ((var? v) (extend v u))
      ((and (set-pair? u) (set-pair? v)) (unify-sets u v st))
      ((and (pair? u) (pair? v))
       (let*-bind ([st.added-car (unify (car u) (car v) st)]
                   [st.added-cdr (unify (cdr u) (cdr v) (car st.added-car))])
         (cons (car st.added-cdr) (append (cdr st.added-car) (cdr st.added-cdr)))))
      ((equal? u v) (cons st empty-state))
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
      ((nonempty-set? v)
       (or (ormap (lambda (el) (occurs-check x el S))
                  (set-head v))
           (occurs-check x (set-tail v) S)))
      ((pair? v)
       (or (occurs-check x (car v) S)
           (occurs-check x (cdr v) S)))
      (else #f))))

; Var, Term, Substitution -> (Streamof (Pair Substitution (Listof Association)))
(define (ext-s-check x v S)
  (if (occurs-check x v S)
      #f
      (cons (subst-add S x v) (list (cons x v)))))

; (Listof Association), Substitution -> UnificationResult
(define (unify* S+ st)
  (unify (map lhs S+) (map rhs S+) st))

; Search

; For some type a ∌ #f, Procedure, (Pair b Procedure)
; (Streamof a): (U #f (SuspendedStreamof a) a (Pair a (SuspendedStreamof a)))
; (SuspendedStreamof a): (-> (Streamof a))
; SearchStream: (Streamof State)
; SuspendedStream: (SuspendedStreamof State)
; (Expansion a): (a -> (Streamof a))
; Goal: (Expansion State)

; Match on search streams.
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

; (Streamof a) -> (or/c (values a (Streamof a)) (values #f #f))
(define (head+tail f)
  (case-inf f
    [()    (values #f #f)]
    [(f)   (head+tail (f))]
    [(c)   (values c #f)]
    [(c f) (values c f)]))

(define (list->stream lst)
  (cond
   [(null? lst) #f]
   [(pair? lst) (let ([rest (list->stream (cdr lst))])
                  (cons (car lst) (suspend rest)))]
   [eler (error 'list->stream "Cannot convert non-list to stream" lst)]))

(define (stream . xs)
  (list->stream xs))

; (suspend e:(Streamof a)) -> (SuspendedStreamof a)
; Used to clearly mark the locations where search is suspended in order to
; interleave with other branches.



; Constraints
; C refers to the constraint store map
; c refers to an individual constraint record

; SimpleConstraint: Goal
;

; Previously, SimpleConstraints where not traditional goals, see `and-foldl`.
; They've been updated to be traditional goals instead, see `bind-foldl`.
; Invariants assumed for type constraints:
; 1. Must be positive, not negative. not-pairo wouldn't work.
; 2. Each type must have infinitely many possible values to avoid
;      incorrectness in combination with disequality constraints,
;      like: (fresh (x) (booleano x) (=/= x #t) (=/= x #f))
; 3. Types must be disjoint from each other

; Predicate: Any -> (or #f Any)
; CompareResult: (or '< '> '=)
; Ordering: T, T -> CompareResult where T is defined by the corresponding predicate.
; Propagator: T -> SimpleConstraint

; Predicate Symbol CompareResult (or #f Propagator) -> TypeConstraint
(define type-constraint
  (case-lambda
    [(predicate reified ordering)
     (type-constraint predicate reified ordering #f)]
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
    (conj
     (infer-setso u)
     (lambda (st)
       (let ((type-pred (type-constraint-predicate tc))
             (type-prop (or (type-constraint-propagator tc)
                            atomic-propagator)))
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
             (else #f))))))))

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

(define (listlike-constraint rec null? pair? first rest)
  (values
   (lambda (x y)
     (cond
      [(and (null? x) (null? y)) '=]
      [(and (null? x) (pair? y)) '<]
      [(and (pair? x) (null? y)) '>]
      [(and (pair? x) (pair? y))
       (let ((r (lex-compare (first x) (first y))))
         (if (eq? r '=)
             (lex-compare (rest x) (rest y))
             r))]
      [(error 'listlike-compare "cannot compare objects of differing types" x y)]))
   (lambda (x)
     (cond
      [(null? x) succeed]
      [(pair? x) (rec (rest x))]
      [else      fail]))))

(define-syntax late-bound
  (syntax-rules ()
    [(late-bound fn)
     (lambda as (apply fn as))]))

(define-values (set-compare valid-seto)
  (listlike-constraint (late-bound seto)  set-null? set-pair? set-first set-rest))
(define-values (list-compare valid-listo)
  (listlike-constraint (late-bound listo) null?     pair?     car       cdr))

(define pair-or-null? (disjoin pair? null?))

(declare-type-constraints type-constraints
  (numbero number?       num number-compare)
  (stringo string?       str string-compare)
  (symbolo symbol?       sym symbol-compare)
  (listo   pair-or-null? lst list-compare   valid-listo)
  (seto    set?          set set-compare    valid-seto))

;; Var, (Listof Association) -> Goal
;; Add the primitive constraint that at least one of the associations doesn't hold
(define (add-to-D var assoc-list)
  (assert (pair? assoc-list))
  (lambda (st)
    (let* ([c   (lookup-c st var)]
           [c^  (c-with-D c (cons assoc-list (c-D c)))]
           [st^ (set-c st var c^)])
      (if (pair? (cdr assoc-list))
          st^
          (foldl
           (lambda (u st)
             (let ([t (rhs (car assoc-list))])
               (cond
                [(find (lambda (v) (var-eq? var v)) u)
                 =>
                 (lambda (Z)
                   (bind st
                         (fresh (N)
                           (apply uniono u)
                           (conde
                            [(ino N Z) (!ino N t)]
                            [(ino N t) (!ino N Z)]
                            [(== Z ∅) (=/= t ∅)]))))]
                [else
                 (error 'add-to-D "Invalid uniono constraint over variable" u var)])))
           st^
           (c-U c))))))

(define (atom? obj)
  (or (set-null? obj)
      (null? obj)
      (symbol? obj)
      (char? obj)
      (string? obj)
      (number? obj)))

;; (Listof Association) -> Goal
(define (=/=* S+)
  (=/= (map lhs S+) (map rhs S+)))

#|
Currently, disunification is written using a naive algorithm
which often uses the system's disjunction when the disunification constraint
itself contains a notion of disjunction. However, there are cases in which
we do need to generate a genuine goal and therefore use more typical notions of
disjunction. In order to rectify this, we use a partially free structure which includes
both a number of first-order goals constructors and a higher-order goal constructors,
to which we fall back in the more general case.

Free-Goal: (U Higher-Order-Goal  Free-Disjunction Free-Conjunction Free-Disunification)
Higher-Order-Goal: (cons/c 'goal Goal)
Free-Disjunction: (cons/c 'disj (listof Free-Goal))
Free-Conjunction: (cons/c 'conj (listof Free-Goal))
Free-Disunification: (cons/c '=/= (listof Free-Goal))
|#

(define (tagged-list-predicate tag)
  (lambda (obj)
    (and (list? obj) (pair? obj) (eq? (car obj) tag))))

(define singleton? (conjoin pair? (compose null? cdr)))

(define free-disjunction?    (tagged-list-predicate 'disj))
(define free-conjunction?    (tagged-list-predicate 'conj))
(define free-disunification? (tagged-list-predicate '=/=))
(define free-success?        (conjoin free-conjunction? singleton?))
(define free-failure?        (conjoin free-disjunction? singleton?))
(define free-subterms        cdr)
(define higher-order-goal?   (tagged-list-predicate 'goal))

(define (opposite-end end)
  (case end
    [(disj) 'conj]
    [(conj) 'disj]
    [else   (error 'opposite-end "Invalid end" end)]))

(define (end->constructor end)
  (case end
    [(disj) disj]
    [(conj) conj]
    [else   (error 'end->constructor "Invalid end" end)]))

(define (higher-order-goal goal) `(goal ,goal))

(define free-failure '(disj))
(define free-success '(conj))

(define (free-disunification left right)
  (assert (var? left))
  (list '=/= left right))

(define (free-extrema higher-end)
  (define lower-end (opposite-end higher-end))
  (define join? (tagged-list-predicate higher-end))
  (define meet? (tagged-list-predicate lower-end))
  (define top   (list lower-end))
  (define bot   (list higher-end))
  (lambda objs
    (define (free-join2 left right)
      (cond
       [(or (equal? top left) (equal? top right))
        top]
       [(equal? bot left)  right]
       [(equal? bot right) left]
       [(and (join? left) (join? right))
        (append (list higher-end) (free-subterms left) (free-subterms right))]
       [(or (higher-order-goal? left) (higher-order-goal? right))
        (higher-order-goal
         ((end->constructor higher-end)
          (free-goal->higher-order-goal left)
          (free-goal->higher-order-goal right)))]
       [(join? left)  `(,higher-end ,@(free-subterms left) ,right)]
       [(join? right) `(,higher-end ,left ,@(free-subterms right))]
       [else          `(,higher-end ,left ,right)]))
    (reduce free-join2 bot objs)))

(define free-disjunction (free-extrema 'disj))
(define free-conjunction (free-extrema 'conj))

;; (Listof Association) -> Goal
;; Takes a list of (atomic) associations and returns a goal
;; which ensure at least one of them does not hold.
(define (primitive-disunification disequalities)
  (cond
   [(null? disequalities) fail]
   [(pair? disequalities)
    (let* ((activated-association (car disequalities))
           (activated-variables (filter
                                 var?
                                 (list (lhs activated-association)
                                       (rhs activated-association)))))
      (apply conj
             (map (lambda (var)
                    (add-to-D var disequalities))
                  activated-variables)))]
   [else (error 'primitive-disunification "Requires a list of associations")]))

;; Free-Goal -> Goal
(define (free-goal->higher-order-goal gl)
  (cond
   [(and (free-disjunction? gl)
         (ormap free-disunification? (free-subterms gl)))
    (let ([disunifications (filter free-disunification? (free-subterms gl))]
          [other-goals (filter (negate free-disunification?) (free-subterms gl))])
      (apply
       disj
       (primitive-disunification
        (map (compose list->pair free-subterms) disunifications))
       (map free-goal->higher-order-goal other-goals)))]
   [(free-disjunction? gl)
    (apply disj (map free-goal->higher-order-goal (free-subterms gl)))]
   [(free-conjunction? gl)
    (apply conj (map free-goal->higher-order-goal (free-subterms gl)))]
   [(free-disunification? gl)
    (let ([subterms (free-subterms gl)])
      (primitive-disunification (list (cons (car subterms) (cadr subterms)))))]
   [(higher-order-goal? gl)
    (cadr gl)]
   [else (error 'free-goal->higher-order-goal "Invalid free goal" gl)]))

;; Term, Term, Substitution -> Free-Goal
;; Given two terms and a substitution, return a free goal which
;; implements the disunification of the two terms given that substitution
(define (disunify left right subst)
  (let ([left  (walk left subst)]
        [right (walk right subst)])
    (cond
     [(equal? left right)
      free-failure]
     [(and (atom? left) (atom? right))
      free-success]
     [(and (pair? left) (pair? right))
      (free-disjunction
       (disunify (car left) (car right) subst)
       (disunify (cdr left) (cdr right) subst))]
     [(and (set-pair? left) (set-pair? right))
      (higher-order-goal
       (fresh (N)
         (conde
          [(ino N left) (!ino N right)]
          [(ino N right) (!ino N left)])))]
     [(not (or (var? left) (var? right)))
      free-success]
     [(and (var? right) (not (var? left)))
      (disunify right left subst)]
     [(var-eq? left right)
      free-failure]
     [else
      (free-disunification left right)])))

(define-syntax defrel-primitive
  (syntax-rules ()
    [(defrel-primitive (name arg args ...) body ...)
     (define (name arg args ...)
       (conj
        (infer-setso arg)
        (infer-setso args) ...
        body ...))]))

;; Term, Term -> Goal
;; Holds IFF and the left and right objects are different
(defrel-primitive (=/= left right)
  (lambda (st)
    ((free-goal->higher-order-goal (disunify left right (state-S st))) st)))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambda (st)
       (let ((x (walk* x (state-S st))) ...)
         ((fresh () g g* ...) st))))))

(define-syntax project0
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambda (st)
       (let ((x (walk x (state-S st))) ...)
         ((fresh () g g* ...) st))))))

;; Term, Term -> Goal
;; Holds when `i` is a member of `s`
(define (ino i s)
  (project0 (s)
    (cond
     [(set-pair? s)
      (conde
       ;; possible duplicate introduced here
       [(== i (set-first s))]
       [(ino i (set-rest s))])]
     [(var? s)
      (fresh (k)
        (seto k)
        (== s (set-cons i k)))]
     [else          fail])))

(define (cons-new elem lst)
  (if (member elem lst)
      lst
      (cons elem lst)))

;; State, Var, Term -> State
;; Add the constraint that a variable musn't unify to a term to a state
(define (add-to-M st v trm)
  (let* ((c  (lookup-c st v))
         (c^ (c-with-M c (cons-new trm (c-M c)))))
    (set-c st v c^)))

;; Term, Term -> Goal
;; Goal which succeeds iff the item is not a member of a given set
(defrel (!ino i s)
  (infer-setso i)
  (seto s) ; already infers sets
  (project0 (s)
    (cond
     [(set-pair? s)
      (fresh ()
       (=/=  i (set-first s))
       (!ino i (set-rest s)))]
     [(var? s)
      (lambda (st)
        (add-to-M st s i))]
     [else          succeed])))

;; State, Var, Var -> State
;; Add the constraint that two variables are mutually exclusive sets
(define (add-to-E st u v)
  (define (add-E st v x)
    (let* ((c  (lookup-c st v))
           (c^ (c-with-E c (cons-new x (c-E c)))))
      (set-c st v c^)))
  (add-E (add-E st u v) v u))

;; Term, Term -> Goal
;; Goal which succeeds only when both arguments are disjoint sets
(defrel (disjo p q)
  (seto p)
  (seto q)
  (project0 (p q)
    (cond
     [(or (null-set? p) (null-set? q))
      succeed]
     [(equal? p q)
      (conj (== p (set)) (== q (set)))]
     [(set-pair? p)
      (conj (!ino (set-first p) q) (disjo (set-rest p) q))]
     [(set-pair? q)
      (conj (!ino (set-first q) p) (disjo (set-rest q) p))]
     [(and (var? p) (var? q))
      (lambda (st)
        (add-to-E st p q))]
     [else fail])))

;; Term, Term -> Goal
;; Generalized 'absento': 'term1' can be any legal term (old version
;; of faster-miniKanren required 'term1' to be a ground atom).
(defrel (absento term1 term2)
  (=/= term1 term2)
  (sub-absento term1 term2))

;; Term, Term -> Goal
;; Asserts that `term1` cannot occur in any subterm of `term2`
(defrel-primitive (sub-absento term1 term2)
  (project0 (term1 term2)
    (cond
     [(pair? term2)
      (conj (absento term1 (car term2))
            (absento term1 (cdr term2)))]
     [(set-pair? term2)
      (conj (absento term1 (set-first term2))
            (sub-absento term1 (set-rest term2)))]
     [(var? term2)
      (lambda (st)
        (define c (lookup-c st term2))
        (define sub-absent (c-B c))
        (define bound      (or (c-L c) '()))
        (define c^ (c-with-B c (cons-new term1 sub-absent)))
        (define st^ (set-c st term2 c^))
        ((apply conj (map (lambda (k.v)
                            (conj (absento term1 (car k.v))
                                  (absento term1 (cdr k.v))))
                          bound))
         st^))]
     [else succeed])))

(defrel (union+o l r l+r)
  (disjo l r)
  (uniono l r l+r))

(defrel (removeo set elem set-elem)
  (== set (set-cons elem set-elem))
  (!ino elem set-elem))

(define (add-to-U st p q r)
  (define new-constraint (list p q r))
  (define (add-U st v)
    (let* ((c  (lookup-c st v))
           (c^ (c-with-U c (cons-new new-constraint (c-U c)))))
      (set-c st v c^)))
  (add-U (add-U (add-U st p) q) r))

(defrel (uniono l r l+r)
  (seto l)
  (seto r)
  (seto l+r)
  (project0 (l r l+r)
    (cond
     [(equal? l r)    (== l l+r)]
     [(null-set? l+r) (fresh ()
                        (== l ∅)
                        (== r ∅))]
     [(null-set? l)   (== r l+r)]
     [(null-set? r)   (== l l+r)]
     [(set-pair? l+r)
      (let ([t1 (set-first l+r)]
            [t2 (set-rest l+r)])
        (fresh (N N1 N2)
          (seto N)
          (seto N1)
          (seto N2)
          (removeo l+r t1 N)
          (conde
           [(removeo l t1 N1) (uniono N1 r N)]
           [(removeo r t1 N1) (uniono l N1 N)]
           [(removeo l t1 N1) (removeo r t1 N2) (uniono N1 N2 N)])))]
     [(and (not (set-pair? l)) (set-pair? r))
      (uniono r l l+r)]
     [(set-pair? l)
      (let ([t1 (set-first l)]
            [t2 (set-rest l)])
        (fresh (N N1 N2)
          (seto N)
          (seto N1)
          (seto N2)
          (removeo l   t1 N1)
          (removeo l+r t1 N)
          (conde
           [(!ino t1 r) (uniono N1 r N)]
           [(removeo r t1 N2) (uniono N1 N2 N)])))]
     [(and (var? l) (var? r) (var? l+r))
      (lambda (st)
        (add-to-U st l r l+r))]
     [else fail])))

;; freeo constraint
(defrel (freeo key alist)
  (infer-setso key)
  (listo alist)
  (project0 (alist)
    (cond
     [(null? alist)
      succeed]
     [(var? alist)
      (lambda (st)
        (define c     (lookup-c st alist))
        (define free  (or (c-F c) '()))
        (define bound (or (c-L c) '()))
        (define c^    (c-with-F c (cons-new key free)))
        (define st^   (set-c st alist c^))
        ((apply conj (map (lambda (k.v) (=/= key (car k.v)))
                          bound))
         st^))]
     [(pair? alist)
      (fresh (h t)
        (== (car alist) (cons h t))
        (=/= h key)
        (freeo key (cdr alist)))]
     [else fail])))

;; Lookup constraint
(defrel (lookupo key alist value)
  (infer-setso key)
  (infer-setso value)
  (listo alist)
  (project0 (alist)
    (cond
     [(null? alist)
      fail]
     [(pair? alist)
      (let ([a (car alist)] [d (cdr alist)])
        (fresh (K V)
          (== a (cons K V))
          (conde
           [(== K key)  (== V value)]
           [(=/= K key) (lookupo key d value)])))]
     [(var? alist)
      (lambda (st)
        (define c          (lookup-c st alist))
        (define free       (or (c-F c) '()))
        (define bound      (or (c-L c) '()))
        (define sub-absent (c-B c))
        (define c^         (c-with-L c (cons (cons key value) bound)))
        ((conj
          (apply conj (map (lambda (k) (=/= k key))
                           free))
          (apply conj (map (lambda (k.v)
                             (disj (=/= key (car k.v))
                                   (== (cdr k.v) value)))
                           bound))
          (apply conj (map (lambda (s)
                             (conj (absento s key)
                                   (absento s value)))
                           sub-absent))
          (lambda (st)
            (set-c st alist c^)))
         st))])))

;; Derived set constraints
(define (!uniono l r l+r)
  (fresh (N)
    (conde
     [(ino N l+r)  (!ino N l) (!ino N r)]
     [(!ino N l+r) (disj (ino N r) (ino N l))])))

(define (!disjo p q)
  (fresh (N)
    (ino N p)
    (ino N q)))

(define (subseteqo small big)
  (uniono small big big))

(defrel (subseto small big)
  (uniono small big big)
  (fresh (x)
    (ino x big)
    (!ino x small)))

(defrel (subtracto s1 obj s2)
  (seto s1)
  (seto s2)
  (!ino obj s2)
  (conde
   [(== s1 (set-cons obj s2))]
   [(== s1 s2)]))

(defrel (singletono obj st)
  (== st (set obj)))

; (State, Any -> SimpleConstraint), State, List -> SimpleConstraint
; Fold lst with proc and initial value init. If proc ever returns #f,
; return with #f immediately. Used for applying a series of
; constraints to a state, failing if any operation fails.
(define (and-foldl proc init lst)
  (if (null? lst)
    init
    (let ((res (proc (car lst) init)))
      (and res (and-foldl proc res (cdr lst))))))

;; a, (Listof (Expander a)) -> (Streamof a)
;; Applies a list of goals to an initial state
;; `(bind-foldl state (list goal ...))` is equivalent to `(bind* state goal ...)`,
;; but is a procedure, rather than syntax
(define (bind-foldl state goals)
  ((apply conj goals) state))

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
    [()    strm]
    [(f)   (suspend (map-inf proc (f)))]
    [(c)   (proc c)]
    [(c f) (cons (proc c) (suspend (map-inf proc (f))))]))

;; Term -> Listof Term
;; This walks the subterms of a given term, and returns a list of
;; subterms which must be sets.
(define (infer-set-constraints term)
  (cond
   [(pair? term) (append (infer-set-constraints (car term))
                         (infer-set-constraints (cdr term)))]
   [(nonempty-set? term)
    (let ([con-head (append-map infer-set-constraints (set-head term))]
          [con-tail (infer-set-constraints (set-tail term))])
      (cons (set-tail term) (append con-tail con-head)))]
   [else         '()]))

;; Term -> Goal
;; Walks the term structure, ensuring
(define (infer-setso term)
  (apply conj (map seto (infer-set-constraints term))))


(defrel-primitive (== u v)
  (lambda (st)
    (let*-bind ([st^.added (unify u v st)])
      (let* ([st^    (car st^.added)]
             [added  (cdr st^.added)])
        (bind-foldl st^ (map update-constraints added))))))

(define (replace from to list)
  (cond
   [(null? list)             list]
   [(equal? from (car list)) (cons to (replace from to (cdr list)))]
   [else                     (cons (car list) (replace from to (cdr list)))]))

;; Association -> Goal
;; Given a new association, return a goal which is its logical consequence,
;; WRT the given constraint
(define (update-constraints a)
  ;; Not fully optimized. Could do sub-absento update with fewer
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
                        (map (lambda (atom) (sub-absento atom (rhs a))) (c-B old-c))
                        (map =/=* (c-D old-c))
                        (map (lambda (mem) (!ino mem (rhs a))) (c-M old-c))
                        (map (lambda (vr)
                               (assert (var? vr))
                               (fresh ()
                                 (disjo (rhs a) vr)
                                 (lambda (st)
                                   (let* ([vr-old-c (lookup-c st vr)]
                                          [vr-new-c-E (remove (lhs a) (c-E vr-old-c))])
                                     (set-c st vr (c-with-E vr-old-c vr-new-c-E))))))
                             (c-E old-c))
                        (map (lambda (con)
                               (fresh ()
                                 (lambda (st)
                                   (bind-foldl
                                    st
                                    (map
                                     (lambda (vr)
                                       (lambda (st)
                                         (let* ([vr-old-c   (lookup-c st vr)]
                                                [vr-new-c-U (remove con (c-U vr-old-c))])
                                           (set-c st vr (c-with-U vr-old-c vr-new-c-U)))))
                                     con)))
                                 (apply uniono (replace (lhs a) (rhs a) con))))
                             (c-U old-c))
                        (map (lambda (mem) (freeo mem (rhs a)))
                             (or (c-F old-c) '()))
                        (map (lambda (k.v) (lookupo (car k.v) (rhs a) (cdr k.v)))
                             (or (c-L old-c) '())))))))))


(define (walk* v S)
  (let ((v (walk v S)))
    (cond
      ((var? v) v)
      ((set-pair? v)
       (set-cons (walk* (set-first v) S) (walk* (set-rest v) S)))
      ((pair? v)
       (cons (walk* (car v) S) (walk* (cdr v) S)))
      (else v))))

(define succeed (lambda (x) x))
(define fail (lambda _ #f))

; Reification

; S - substitution
; T - type constraints
; B - sub-absento constraints
; A - absento constriants
; D - disequality constraints
; M - non-membership constraints
; E - mutual exclusivity constraints
; U - union constraints
; F - free-binding constraints
; L - lookup constraints

(define (reify x)
  (lambda (st)
    (let* ((S (state-S st))
           (v (walk* x S))
           (R (reify-S v (subst empty-subst-map nonlocal-scope)))
           (relevant-vars (vars v))
           (v (simplify-S (walk* v R) R)))
      (let*-values (((T D B M E U F L) (extract-and-normalize st relevant-vars x))
                    ((D B M E U F L) (drop-irrelevant D B M E U F L relevant-vars))
                    ((D B A)   (drop-subsumed D B st)))
        (form v
              (walk* D R)
              (walk* T R)
              (walk* B R)
              (walk* A R)
              (walk* M R)
              (walk* E R)
              (walk* U R)
              (walk* F R)
              (walk* L R))))))

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
                     (filter-map (normalize-diseq st) (c-D (lookup-c st x))))
                   relevant-vars)))
  (define B (append*
              (map (lambda (x)
                     (map (lambda (b-lhs)
                            (cons (walk* b-lhs (state-S st))
                                  x))
                          (c-B (lookup-c st x))))
                   relevant-vars)))
  (define M (append*
             (map (lambda (x)
                    (map (lambda (m-lhs)
                           (cons (walk* m-lhs (state-S st))
                                 x))
                         (c-M (lookup-c st x))))
                  relevant-vars)))
  (define E (append*
             (map (lambda (x)
                    (map (lambda (e-lhs)
                           (cons (walk* e-lhs (state-S st))
                                 x))
                         (c-E (lookup-c st x))))
                  relevant-vars)))
  (define U (append*
             (map (lambda (x)
                    (map (lambda (u-lhs)
                           (cons (walk* u-lhs (state-S st))
                                 x))
                         (c-U (lookup-c st x))))
                  relevant-vars)))
  (define F (append*
             (map (lambda (x)
                    (map (lambda (f-lhs)
                           (cons (walk* f-lhs (state-S st))
                                 x))
                         (or (c-F (lookup-c st x)) '())))
                  relevant-vars)))
  (define L (append*
             (map (lambda (x)
                    (map (lambda (l-lhs)
                           (cons (walk* l-lhs (state-S st))
                                 x))
                         (or (c-L (lookup-c st x)) '())))
                  relevant-vars)))
  (values T D B M E U F L))

(define (normalize-diseq st)
  (lambda (S+)
    ;; This is guaranteed to terminate, as unify's stream is always finite
    (define cases (take #f (unify* S+ st)))
    (cond
     [(null? cases)       #f]
     [(null? (cdr cases)) (walk* (cdar cases) (state-S st))]
     [else                (walk* S+ st)])))

; Drop constraints that are satisfiable in any assignment of the reified
; variables, because they refer to unassigned variables that are not part of
; the answer, which can be assigned as needed to satisfy the constraint.
(define (drop-irrelevant D B M E U F L relevant-vars)
  (define (all-relevant? t)
    (andmap (lambda (v) (member v relevant-vars))
            (vars t)))
  (values (filter all-relevant? D)
          (filter all-relevant? B)
          (filter all-relevant? M)
          (filter all-relevant? E)
          (filter all-relevant? U)
          (filter all-relevant? F)
          (filter all-relevant? L)))

;; Given two lists and an optional equality procedure between their elements,
;; returns three values:
;; - All the elements of the first which are not equal to any in the second
;; - All the elements of the first which are equal to any in the second
;; - All the elements of the second which are not equal to any in the first
;; If the equality procedure isn't provided, it's assumed to be `equal?`
;; Whether or not the lists have duplicates or are ordered is undefined
(define list-differences
  (case-lambda
    [(fst snd) (list-differences fst snd equal?)]
    [(fst snd =)
     (if (null? snd)
         (values fst '() snd)
         (apply values
                (fold-left
                 (lambda (acc el)
                   (if (memp (lambda (p) (= el p)) snd)
                       (list (car acc)
                             (cons el (cadr acc))
                             (filter (lambda (p) (not (= el p)))
                                     (caddr acc)))
                       (list (cons el (car acc))
                             (cadr acc)
                             (caddr acc))))
                 (list '() '() snd)
                 fst)))]))

(define (drop-subsumed D B st)
  (define (b=d? V.K L_K.V)
    (and (eqv? (length L_K.V) 1)
         (or (and (var-eq? (caar L_K.V) (cdr V.K))
                  (equal? (car V.K) (cdar L_K.V)))
             (and (var? (cdar L_K.V))
                  (var-eq? (cdar L_K.V) (cdr V.K))
                  (var-eq? (caar L_K.V) (car V.K))))))

  (define B^ (rem-subsumed
                 b-subsumed-by?
                 (filter (lambda (b)
                           (not (or (sub-absento-rhs-atomic? b st)
                                    (sub-absento-rhs-occurs-lhs? b st))))
                         B)))

  ;; To keep backwards compatability with the existing reification visualization,
  ;; we rewrite sub-absento constraints into absento,
  ;; even if it means providing redundant information.
  ;;
  ;; For example:
  ;; ```
  ;; > (run* (p) (symbolo q) (absento 1 q))
  ;; ((_.0 (set _.0) (absento (1 _.0))))
  ;; ```
  ;;
  ;; In this case, we implicitly provide the redundant information that
  ;; `(=/= 1 q)`, which is obvious from the disjointness of types.
  ;; If we had only the `(=/= 1 q)` constraint, then we'd remove it
  ;; as being subsumed by the type constraint, but we use it when deciding
  ;; whether to use `absento` or `sub-absento` constraints.
  ;;
  ;; To do that, we recover the list of implicit disequalities
  ;; which are subsumed by the disjointness of types in absento
  (define Dv
    (filter-map
      (lambda (b)
        (let ((d (absento->diseq b)))
          (and (d-subsumed-by-T? d st) d)))
      B))

  (define-values (B^^ A^ D^) (list-differences B^ (append D Dv) b=d?))

  (define D^^ (rem-subsumed
                  d-subsumed-by?
                  (filter (lambda (d)
                            (not (or (d-subsumed-by-T? d st)
                                     (d-subsumed-by-B? d B st))))
                          D^)))

  (values D^^ B^^ A^))

;; Holds for type-constraint TYP iff the constraint is either
;; unbound or a compound type constraint (one with a non-atomic propagator)
(define (compound-type-constraint? typ)
  (or (unbound? typ)
      (type-constraint-propagator typ)))

;; Constraints for which the RHS is atomic (has no sub-components),
;; which hold trivially
(define (sub-absento-rhs-atomic? b st)
  (not (and (var? (rhs b))
            (compound-type-constraint? (var-type (rhs b) st)))))

;; Sub-absento constraints that are trivially satisfied because
;; any violation would cause a failure of the occurs check.
;; Example:
;;  (sub-absento (list x y z) x) is trivially true because a violation would
;;  require x to occur within itself.
(define (sub-absento-rhs-occurs-lhs? b st)
  (occurs-check (rhs b) (lhs b) (state-S st)))

;; Drop disequalities that are subsumed by an absento contraint
;; that is not itself equivalent to just a disequality.
(define (d-subsumed-by-B? d A st)
  (exists (lambda (a)
            (and (not (sub-absento-rhs-atomic? a st))
                 (d-subsumed-by? d (list a))))
          A))

; Drop disequalities that are fully satisfied because the types are disjoint
; either due to type constraints or ground values.
; Examples:
;  * given (symbolo x) and (numbero y), (=/= x y) is dropped.
(define (d-subsumed-by-T? d st)
  (exists (lambda (pr)
            (not
             (or (and (var? (lhs pr)) (var-types-match? (lhs pr) (rhs pr) st))
                 (and (var? (rhs pr)) (var-types-match? (rhs pr) (lhs pr) st)))))
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
(define (b-subsumed-by? t1 t2)
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
  (define unifications (map-inf car (unify* d1
                                            (state
                                             (subst empty-subst-map nonlocal-scope)
                                             empty-C))))

  (fold-inf
   (lambda (acc elem) (or acc elem))
   #f
   (let*-bind ((st       unifications)
               (st+.added (unify* d2 st)))
     (null? (cdr st+.added)))))

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

(define (form v D T B A M E U F L)
  (let ((ft (filter-map
              (lambda (p)
                (let ((tc-type (car p)) (tc-vars (cdr p)))
                  (and (not (null? tc-vars))
                       `(,tc-type . ,(sort-lex tc-vars)))))
              T))
        (fd (sort-D D))
        (fb (sort-lex B))
        (fa (sort-lex A))
        (fm (remove-duplicates (sort-lex M)))
        (fe (remove-duplicates (sort-E E)))
        (fu (remove-duplicates (sort-U U)))
        (ff (remove-duplicates (sort-lex F)))
        (fl (remove-duplicates (sort-lex L))))
    (let ((fd (if (null? fd)
                  fd
                  (let ((fd (drop-dot-D fd)))
                    `((=/= . ,fd)))))
          (fb (if (null? fb)
                  fb
                  (let ((fb (drop-dot fb)))
                    `((sub-absento . ,fb)))))
          (fa (if (null? fa)
                  fa
                  (let ((fa (drop-dot fa)))
                    `((absento . ,fa)))))
          (fm (if (null? fm)
                  fm
                  `((∉ . ,(drop-dot fm)))))
          (fe (if (null? fe)
                  fe
                  `((∥ . ,(drop-dot fe)))))
          (fu (if (null? fu)
                  fu
                  `((∪₃ ,@fu))))
          (ff (if (null? ff)
                  ff
                  `((free . ,(drop-dot ff)))))
          (fl (if (null? fl)
                  fl
                  `((lookup . ,(drop-dot-L fl))))))
      (cond
       ((and (null? fd) (null? ft) (null? fb) (null? fa)
             (null? fm) (null? fe) (null? fu) (null? ff)
             (null? fl)
             (not (always-wrap-reified?)))
        v)
       (else (append `(,v) fd ft fb fa fm fe fu ff fl))))))

(define (sort-U U)
  (map car U))

(define (sort-E E)
  (list-sort
    (lambda (x y)
      (lex<=? (car x) (car y)))
    (map sort-pr E)))

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

(define (drop-dot-L obj)
  (map
   (lambda (o) (list (caar o) (cdr o) (cdar o)))
   obj))

; (Listof (Pair Predicate Ordering))
(define type-orderings
  (append
    ; compound and atomic types
    (map (lambda (tc) (cons (type-constraint-predicate tc)
                            (type-constraint-ordering tc)))
         type-constraints)
    `(; booleans
      (,(lambda (v) (eq? v #f)) . ,(lambda (x y) '=))
      (,(lambda (v) (eq? v #t)) . ,(lambda (x y) '=)))))

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
      (errorf 'type-index "missing ordering for type of value ~s" v))))

; (Term, Term) -> (or CompareResult error)
; defined when arguments are types addressed by type-constraints;
; see type-orderings.
(define (lex-compare x y)
  (let-values (((x-o-idx x-o) (type-ordering x))
               ((y-o-idx y-o) (type-ordering y)))
    (if (eqv? x-o-idx y-o-idx)
      (x-o x y)
      (number-compare x-o-idx y-o-idx))))

(define (lex<=? x y)
  (member (lex-compare x y) '(< =)))

;; Remove duplicates from a list and sort them
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
           [t (simplify-S (set-tail s) sub)])
      (make-set h t))]
   [else obj]))

(define (take-unique k strm)
  (define seen-values (make-hashtable equal-hash equal?))
  (let loop ([k k] [strm strm])
    (and (not (and k (zero? k)))
         (case-inf strm
           [()    #f]
           [(f)   (loop k (f))]
           [(c)   (hashtable-set! seen-values c #t)]
           [(c f) (if (hashtable-contains? seen-values c)
                      (loop k (f))
                      (begin
                        (hashtable-set! seen-values c #t)
                        (loop (and k (sub1 k)) (f))))])))
  (sort-lex (vector->list (hashtable-keys seen-values))))

(define-syntax run-unique
  (syntax-rules ()
    [(run-unique n (q) g0 g ...)
     (take-unique n (toplevel-query (q) g0 g ...))]
    [(run-unique n (q0 q1 q ...) g0 g ...)
     (run-unique n (x)
       (fresh (q0 q1 q ...)
         g0 g ...
         (== (list q0 q1 q ...) x)))]))

(define-syntax run-unique*
  (syntax-rules ()
    [(run-unique* (q ...) g g* ...)
     (run-unique #f (q ...) g g* ...)]))
