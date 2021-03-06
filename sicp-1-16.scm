(define (exp1 b n)
  (if (= n 0) 1 (* b (exp1 b (- n 1)))))
(define (exp2 b n)
  (exp2-iter 1 b n))
(define (exp2-iter a b n)
  (if (= n 0) a (exp2-iter (* b a) b (- n 1))))
(define (exp3 b n)
  (cond ((= n 0) 1)
        ((even? n) (exp3 (square b) (/ n 2)))
        (else (* b (exp3 b (- n 1))))))
(define (even? x)
  (= (% x 2) 0))
(define (square x)
  (* x x))
(define (exp4 b n)
  (exp4-iter 1 b n))
(define (exp4-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (exp4-iter a (square b) (/ n 2)))
        (else (exp4-iter (* a b) b (- n 1)))))

(exp1 2 8)
(exp2 2 8)
(even? 11)
(even? 24)
(square 3)
(exp3 2 8)
(exp1 1.0001 100000)
(exp3 1.0001 100000)
(exp4 2 8)
(exp4 1.0001 100000)
