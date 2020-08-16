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

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))


((cubic 2 3 4) 1)
((cubic 2 3 4) 2)

(newton (cubic 2.0 3.0 4.0) 1.0)