(define (deep-rev tree)
  (letrec ((iter (lambda (tree res)
    (if (null? tree)
        res
        (iter (cdr tree)
              (let ((a (car tree)))
                (cons
                  (if (list? a)
                      (deep-rev a)
                      a)
                    res)))))))
    (iter tree '())))
          

(define a '( '(1 2) '(3 4)))
(define b (list 5 (list 6 a)))

a
(deep-rev a)
b
(deep-rev b)