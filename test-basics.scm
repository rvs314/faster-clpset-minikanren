(test "test-zipper"
      (map-zipper list '(1 2 3 4 5))
      '((() 1 (2 3 4 5))
        ((1) 2 (3 4 5))
        ((2 1) 3 (4 5))
        ((3 2 1) 4 (5))
        ((4 3 2 1) 5 ())))

;; Returns a randomized stream which has the same elements/order as LST
;; Inv: (take #f (shaky-stream L)) = L
(define (shaky-stream lst)
  (cond
   [(flip)
    (let ((rst (shaky-stream lst)))
      (suspend rst))]
   [(null? lst)                    #f]
   [(and (null? (cdr lst)) (flip)) (car lst)]
   [else
    (let ((rst (shaky-stream (cdr lst))))
      (cons (car lst)
            (suspend rst)))]))

(let ([strm   (shaky-stream (iota 20))]
      [triple (lambda (el) (shaky-stream (make-list 3 el)))])
  (test "test-bind-foldl"
        (sort < (take #f (bind-foldl strm (list triple triple))))
        (append* (map (lambda (k) (make-list 9 k)) (iota 20)))))

(define-tests conjoin
  [((conjoin even?) 4)                            #t]
  [((conjoin) 'whatever)                          #t]
  [((conjoin number? even?) 2.0)                  #t]
  [((conjoin number? even?) 3.0)                  #f]
  [((conjoin number? even?) 3.0)                  #f]
  [((conjoin number? positive? even? exact?) #\a) #f]
  [((conjoin number? positive? even? exact?) 3)   #f]
  [((conjoin number? positive? even? exact?) 2.0) #f]
  [((conjoin number? positive? even? exact?) -2)  #f]
  [((conjoin number? positive? even? exact?) 2)   #t])

(define-tests disjoin
  [((disjoin even?) 4)                      #t]
  [((disjoin) 'anything)                    #f]
  [((disjoin number? even?) 2.0)            #t]
  [((disjoin number? even?) 3.0)            #t]
  [((disjoin number? positive?) -3)         #t]
  [((disjoin number? string?) #\a)          #f]
  [((disjoin number? positive? exact?) 2.0) #t]
  [((disjoin number? positive? exact?) -2)  #t])

(define-tests reduce
  [(reduce + 0 '())                         0]
  [(reduce + 0 '(1 2 3 4))                  10]
  [(reduce cons '() '(1 2 3))               '(3 2 . 1)]
  [(reduce * 1 '(2 3 4))                    24]
  [(reduce string-append "" '("a" "b" "c")) "cba"]
  [(reduce + 10 '())                        10]
  [(reduce + 10 '(1 2 3))                    6]
  [(reduce cons '(z) '(a b))                '(b . a)])

(define-tests compose
  [((compose) 42)
   42]
  [((compose values) 5)
   5]
  [((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 3)
   8]
  [((compose (lambda (x) (+ x 1)) (lambda (x) (* x 2))) 3)
   7]
  [((compose (lambda (x) x) (lambda (x) x)) 99)
   99]
  [((compose (lambda (x y) (+ x y)) values) 3 4)
   7]
  [((compose (lambda (x y) (* x y)) (lambda (x) (values x x))) 5)
   25]
  [((compose (lambda (x y z) (+ x y z)) (lambda (x) (values x x x))) 1)
   3])
