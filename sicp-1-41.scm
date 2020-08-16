(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(inc 3)
((double inc) 3)
(((double (double double)) inc) 5)

(((double double) inc) 0)
(((double (double double)) inc) 0)
((double (double (double inc))) 0)
; d(dd)i = ddddi
; dddi 
; =21
