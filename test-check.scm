(define test-failed #f)

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
           (failf "~s Failed: Exceded budget of ~a extra answers~%" name)]
          [else
           (let-values ([(next rest) (head+tail answer-stream)])
             (if next
                 (loop rest (remove next expected-results) (add1 i))
                 (failf "~s Failed: Did not find answers: ~s~%" expected-results)))]))))
    ((test-unordered
      name
      (run k (v) g ...)
      results-expr)
     (test-unordered
      name
      (budget 300)
      (run k (v) g ...)
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
