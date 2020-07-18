(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((= (% n d) 0) d)
        (else (find-divisor n (+ d 1)))))
(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e) (% (expmod (square b) (/ e 2) m) m))
        (else (% (* b (expmod b (- e 1) m)) m))))
(define (square x)
  (* x x))
(define (even? x)
  (= (% x 2) 0))

;(define (test-prime n)
  ;(print n)
  ;(start-test n 0))
;(define (start-test n t)
;  1)

;(test-prime 17)

