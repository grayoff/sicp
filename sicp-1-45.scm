(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (even? x) (= (% x 2) 0))

(define (id x) x)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (lambda (x) (f (f x))))

(define (repeated f n)
  (cond ((< n 1) id)
        ((even? n) (double (repeated f (/ n 2))))
        (else (compose f (repeated f (- n 1))))))

(define (fix f guess)
  (let ((y (f guess)))
       (if (< (abs (- y guess)) 0.00001)
           y
           (fix f y))))

(define (avg-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (ntrt n x)
  (fix ((repeated
          avg-damp
          (floor (log2 n)))
        (lambda (y)
          (/ x (expt y (- n 1)))))
       1.0))

(define (log2 x)
  (/ (log x) (log 2)))

(ntrt 2 2.0)
(ntrt 2 4.0)      ; 1
(ntrt 3 8.0)
(ntrt 4 16.0)     ; 2
(ntrt 5 32.0)
(ntrt 6 64.0)
(ntrt 7 128.0)
(ntrt 8 256.0)    ; 3
(ntrt 16 32656.0) ; 4
"done"
(floor (log2 2))