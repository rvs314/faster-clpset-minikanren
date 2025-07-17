(define test-failed #f)

(define quoted?
  (disjoin
   number?
   string?
   boolean?
   symbol?
   bytevector?
   (conjoin pair? (lambda (obj) (eq? (car obj) 'quote)))))

(define-syntax ensure
  (syntax-rules ()
    [(ensure (rator rand ...))
     (let ((rator rator)
           (operands (list rand ...)))
       (unless (apply rator operands)
         (raise-continuable
          (condition
           (make-assertion-violation)
           (make-message-condition "Ensure statement failed")
           (make-irritants-condition
            (map
             (lambda (s o)
               (if (quoted? s)
                   s
                   `(,s => ,o)))
             '(rator rand ...)
             (cons rator operands)))))))]))

(define (failf msg . args)
  (set! test-failed #t)
  (apply printf msg args)
  #f)

(define _.0 '_.0)
(define _.1 '_.1)
(define _.2 '_.2)
(define _.3 '_.3)
(define _.4 '_.4)
(define _.5 '_.5)

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~a\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (failf "Test ~a has failed: ~s~%Expected: ~s~%Computed: ~s~%"
                    title 'tested-expression expected produced)))))))

(define-syntax test-unordered
  (syntax-rules (run budget)
    ((test-unordered
      name
      (budget budget-expr)
      (run k (v) g ...)
      results-expr)
     (let ((budget budget-expr)
           (res results-expr))
       (assert (= (length res) k))
       (printf "Testing ~a~%" name)
       (let loop ([answer-stream (toplevel-query (v) g ...)]
                  [expected-results results-expr]
                  [i 0])
         (cond
          [(null? expected-results)
           (printf "~s Passed: Took ~s extra answers~%" name (- i k))
           #t]
          [(= i budget)
           (failf "~s Failed: Exceded budget of ~a extra answers~%" name budget)]
          [else
           (let-values ([(next rest) (head+tail answer-stream)])
             (if next
                 (loop rest (remove next expected-results) (add1 i))
                 (failf "~s Failed: Did not find answers: ~s~%" name expected-results)))]))))
    ((test-unordered
      name
      (budget budget-expr)
      (run k (v ...) g ...)
      results-expr)
     (test-unordered
      name
      (budget budget-expr)
      (run k (q)
        (fresh (v ...)
          (== q (list v ...))
          g ...))
      results-expr))
    ((test-unordered
      name
      (run k (v ...) g ...)
      results-expr)
     (test-unordered
      name
      (budget 300)
      (run k (v ...) g ...)
      results-expr))))

(define-syntax run-tests
  (syntax-rules ()
    [(_ test-name test-number) (begin)]
    [(_ test-name test-number [expression expected-result] more ...)
     (begin
       (test (format "~a/~a" 'test-name test-number)
             expression expected-result)
       (run-tests test-name (+ 1 test-number) more ...))]))

(define-syntax define-tests
  (syntax-rules ()
    [(_ test-name args ...)
     (run-tests test-name 1 args ...)]))

;; Flips a coin with a 1/K chance of giving heads,
;; where K = 2 by default
(define flip
  (case-lambda
    [()  (flip 2)]
    [(k) (zero? (random k))]))

;; We shadow the normal `take` name, so this
;; is a lazy impl of take on lists
(define (takel k lst)
  (if (zero? k)
      '()
      (cons (car lst)
            (takel (- k 1) (cdr lst)))))

;; Test which where only the first few listed results are actually
;; checked, but the test ensures that the required number of examples
;; exist
(define-syntax test-abridged
  (syntax-rules (run)
    [(test-abridged name
       (run count (v ...)
         body ...)
       expected)
     (begin
       (printf "Testing ~a~%" 'name)
       (let* ((ex expected)
              (rn (run count (v ...) body ...)))
         (unless (= (length rn) count)
           (failf "Test ~a: Expected ~a responses back, but got ~a~%"
                  'name count (length rn)))
         (unless (equal? (takel (length ex) rn) ex)
           (failf "Test ~a: Mismatch in the first ~a results:~%Expected: ~a~%Computed: ~a~%"
                  'name (length ex) ex (takel (length ex) rn))))
       #t)]))
