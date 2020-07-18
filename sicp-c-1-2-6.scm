(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((= (% n d) 0) d)
        (else (find-divisor n (+ d 1)))))
(define (square x)
  (* x x))

(smallest-divisor 5)
(smallest-divisor 15)
(smallest-divisor (* 11 17))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 6)
(prime? 7)
(random 2)