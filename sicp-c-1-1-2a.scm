(define (change x n)
  (cond ((= x 0) 1 )
        ((or (< x 0) (= n 0)) 0)
        (else (+ (change x (- n 1))
                 (change (- x (val n)) n)))))
(define (val n)
  (cond ((= n 1) 1)
        ((= n 2) 5)
        ((= n 3) 10)
        ((= n 4) 25)
        ((= n 5) 50)))
(change 100 5)
