(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q n)
  (cond ((= n 0) b)
        ((even? n) (fib-iter a b
          (+ (square p) (square q))
          (+ (square q) (* 2 p q))
          (/ n 2)))
        (else
  (fib-iter (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q))
            p
            q
            (- n 1)))))
(define (even? x)
  (= (% x 2) 0))
(define (square x)
  (* x x))

(even? 3)
(even? 8)
(square 3)

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 22)
