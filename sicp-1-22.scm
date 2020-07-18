; https://repl.it/@AndreySerikov/sicp-1-22#main.scm

(define (date2runtime date)
  ; HACK
  ; wraps around occasionally!
  (+  
     (* (date-hour date) 60 60 1000) 
     (* (date-minute date) 60 1000) 
     (* (date-second date) 1000) 
     (date-millisecond date)
  )
)
(define (runtime) (date2runtime (current-date)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((= (mod n d) 0) d)
        (else (find-divisor n (+ d 1)))))

(define (square x)
  (* x x))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e) (mod (square (expmod b (/ e 2)) m) m))
        (else (mod (* b (expmod b (- e 1) m)) m))))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n) (report-prime n (- (runtime) start-time)) #f))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (search-for-primes from amount)
  (if (even? from) (search-iter (+ from 1) amount) (search-iter from amount)))

(define (search-iter n amount)
  (if (= amount 0) #t (search-iter (+ n 2) (if (timed-prime-test n) (- amount 1) amount))))


(search-for-primes 1e3 3)
(search-for-primes 1e4 3)
(search-for-primes 1e5 3)
(search-for-primes 1e6 3)
(search-for-primes 1e7 3)
(search-for-primes 1e8 3)
(search-for-primes 1e9 3)

; 1009 *** 3
; 1013 *** 3
; 1019 *** 3
; 10007 *** 8
; 10009 *** 10
; 10037 *** 11
; 100003 *** 21
; 100019 *** 20
; 100043 *** 22
; 1000003 *** 66
; 1000033 *** 66
; 1000037 *** 69
; 10000019 *** 210
; 10000079 *** 212
; 10000103 *** 214
; 100000007 *** 666
; 100000037 *** 666
; 100000039 *** 668
; 1000000007 *** 2109
; 1000000009 *** 2111
; 1000000021 *** 2112

; sqrt(10)    = 3.16227766
; 2111 / 666  = 3,16
; 666 / 212   = 3,14
; 212 / 66    = 3,21
; 66 / 21     = 3,14
; 21 / 10     = 2,1
; 10 / 3      = 3,33

; (timed-prime-test 13)
; (timed-prime-test 15)

; (display (expmod 2 8 200))
; (newline)
