(define (fact1 x)
  (if (= x 1) 1 (* x (fact1 (- x 1)))))
(fact1 170.0)
(define (fact x)
  (fact-iter 1 x))
(define (fact-iter res x)
  (if (= x 1) res (fact-iter (* res x) (- x 1))))
(fact 170.0)
(fact 6)