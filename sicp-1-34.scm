(define (square x) (* x x))

(define (f a) (a 2))

(f square)
(f (lambda (x) (* x (+ x 1))))
(f f)
