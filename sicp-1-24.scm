; https://repl.it/@AndreySerikov/sicp-1-24#main.scm

; system
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

(define (random n) (truncate (* (random-real) (- n 1))))


; divisors
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((= (mod n d) 0) d)
        (else (find-divisor n (next d)))))

(define (next d)
  (if (= d 2) 3 (+ d 2)))

(define (square x)
  (* x x))

(define (prime? n)
  (= (smallest-divisor n) n))


; Fermat test
(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e) (mod (square (expmod b (/ e 2) m)) m))
        (else (mod (* b (expmod b (- e 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


; timed
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10) (report-prime n (- (runtime) start-time)) #f))

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


;(fermat-test 1e9)
;(random 1e8) 53225574
;(expmod 53225574 100000007 100000007) - error
;(* 100000007 100000007)


(search-for-primes 1e3 3)
(search-for-primes 1e4 3)
(search-for-primes 1e5 3)
(search-for-primes 1e6 3)
(search-for-primes 1e7 3)
;(search-for-primes 1e8 3)
;(search-for-primes 1e9 3)

; prime?
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

; prime-next?
; 1009 *** 2
; 1013 *** 2
; 1019 *** 2
; 10007 *** 7
; 10009 *** 4
; 10037 *** 6
; 100003 *** 17
; 100019 *** 12
; 100043 *** 14
; 1000003 *** 41
; 1000033 *** 39
; 1000037 *** 42
; 10000019 *** 129
; 10000079 *** 128
; 10000103 *** 128
; 100000007 *** 407
; 100000037 *** 405
; 100000039 *** 406
; 1000000007 *** 1288
; 1000000009 *** 1306
; 1000000021 *** 1359

; fast-prime?
; 1009 *** 18
; 1013 *** 18
; 1019 *** 25
; 10007 *** 25
; 10009 *** 25
; 10037 *** 16
; 100003 *** 20
; 100019 *** 20
; 100043 *** 18
; 1000003 *** 20
; 1000033 *** 22
; 1000037 *** 22
; 10000019 *** 27
; 10000079 *** 29
; 10000103 *** 37

; 18 / log 1 000 = x / log 1 000 000
; x = 36, but real x = 20...

; 2111 / 1300 = 1,62
; 666 / 405   = 1,64
; 212 / 128   = 1,65
; 68 / 40     = 1,7
; 20 / 14     = 1,42
; 10 / 5      = 2,00
; 2 / 2       = 1,50

; (timed-prime-test 13)
; (timed-prime-test 15)

; (display (expmod 2 8 200))
; (newline)
