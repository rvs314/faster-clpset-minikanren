
(load "./mk-vicare.scm")
(load "./test-check.scm")

(let loop ([i 20])
  (define l
    (let loop ([idx 0] [len 30])
      (if (zero? len)
          '()
          (cons (cons idx (random 200))
                (loop (+ idx 1 (random 256)) (sub1 len))))))
  (test 'list->intmap->list
        (sort (lambda (a b)
                (< (car a) (car b)))
              (intmap->list (list->intmap l)))
        l)
  (when (positive? i)
    (loop (sub1 i))))
