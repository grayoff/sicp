(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (even? x) (= (% x 2) 0))

(define (id x) x)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (lambda (x) (f (f x))))

(define (repeated1 f n)
  (cond ((= n 0) id)
        (else (compose f (repeated f (- n 1))))))

(define (repeated f n)
  (cond ((= n 0) id)
        ((even? n) (double (repeated f (/ n 2))))
        (else (compose f (repeated f (- n 1))))))

(define (smooth f)
  (let ((dx 0.001))
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(define (smooth-n f n)
  ((repeated smooth n) f))

 
((smooth sin) 3.14)
((smooth-n sin 10) 3.14)
(sin 3.14)