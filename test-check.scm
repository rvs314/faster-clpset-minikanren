(define test-failed #f)

(define (failf msg . args)
  (set! test-failed #t)
  (apply printf msg args)
  #f)

(define (log . parts)
  (display parts)
  (newline)
  (cdr (last-pair parts)))

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
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (failf "~s Failed: ~s~%Expected: ~s~%Computed: ~s~%"
                    'title 'tested-expression expected produced)))))))

(define unordered-test-budget 200)

(define-syntax test-unordered
  (syntax-rules (run)
    ((test-unordered
      name
      (run k (v) g ...)
      results-expr)
     (begin
       (printf "Testing ~s~%" name)
       (let loop ([answer-stream (toplevel-query (v) g ...)]
                  [expected-results results-expr]
                  [i 0])
         (cond
          [(null? expected-results)
           (printf "~s Passed: Took ~s extra answers~%" name (- i k))
           #t]
          [(= i unordered-test-budget)
           (failf "~s Failed: Exceded test budget~%" name)]
          [else
           (let-values ([(next rest) (head+tail answer-stream)])
             (if next
                 (loop rest (remove next expected-results) (add1 i))
                 (failf "~s Failed: Did not find answers: ~s~%" expected-results)))]))))))

;; Flips a coin with a 1/K chance of giving heads,
;; where K = 2 by default
(define flip
  (case-lambda
    [()  (flip 2)]
    [(k) (zero? (random k))]))
