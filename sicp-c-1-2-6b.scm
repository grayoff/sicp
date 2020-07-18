(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((= (% n d) 0) d)
        (else (find-divisor n (+ d 1)))))
(define (square x)
  (* x x))

;(smallest-divisor 5)
;(smallest-divisor 15)
;(smallest-divisor (* 11 17))

(define (prime? n)
  (= n (smallest-divisor n)))

;(prime? 6)
;(prime? 7)
;(random 9)

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e) (% (square (expmod b (/ e 2) m)) m))
        (else (% (* b (expmod b (- e 1) m)) m))))
(define (square x)
  (* x x))
(define (even? x)
  (= (% x 2) 0))

(define (fermat-test n)
  (test-it n (+ 1 (random (- n 1)))))
(define (test-it n a)
  (= (expmod a n n) a))


;(expmod 2 8 200)
;(fermat-test 256)


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 10 10)
(fast-prime? 73 10)