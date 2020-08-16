(define (fix f guess)
  (let ((y (f guess)))
    (if (< (abs (- guess y)) 0.0001)
        y
        (fix f y))))

(define (avg a b) (/ (+ a b) 2.0))

(define (avg-damp f)
  (lambda (x) (avg x (f x))))

(define (square x) (* x x))

((avg-damp square) 10)

(define (sqrt x)
  (fix (avg-damp (lambda (y) (/ x y))) 1))

(define (cbrt x)
  (fix (avg-damp (lambda (y) (/ x (square y)))) 1.0))

(define (cube x) (* x x x))

(define (deriv f)
  (let ((dx 0.00001))
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) dx))))

(define (newton-tr g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton g guess)
  (fix (newton-tr g) guess))

(define (sqrt2 x)
  (newton (lambda (y) (- x (square y))) 1.0))

(define (fix-tr f t guess)
  (fix (t f) guess))

(define (sqrt3 x)
  (fix-tr (lambda (y) (/ x y))
          avg-damp
          1.0))

(define (sqrt4 x)
  (fix-tr (lambda (y) (- x (square y)))
          newton-tr
          1.0))

(sqrt 2)
(sqrt 4)
(cbrt 27)

((deriv cube) 5)

(sqrt2 2)
(sqrt2 4)

(sqrt3 2)
(sqrt3 4)

(sqrt4 2)
(sqrt4 4)
