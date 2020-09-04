(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (list? tree)) 1)
        (else
          (+ (count-leaves (car tree))
             (count-leaves (cdr tree))))))

(define a (list (list 1 2) 3 4))
(define b (list a a))

(length a)
(count-leaves a)
(length b)
(count-leaves b)