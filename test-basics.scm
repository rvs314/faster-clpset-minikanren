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
   [(flip)                         (suspend (shaky-stream lst))]
   [(null? lst)                    #f]
   [(and (null? (cdr lst)) (flip)) (car lst)]
   [else                           (cons (car lst)
                                         (suspend (shaky-stream (cdr lst))))]))

(let ([strm   (shaky-stream (iota 20))]
      [triple (lambda (el) (shaky-stream (make-list 3 el)))])
  (test "test-bind-foldl"
        (sort < (take #f (bind-foldl strm (list triple triple))))
        (append* (map (lambda (k) (make-list 9 k)) (iota 20)))))

