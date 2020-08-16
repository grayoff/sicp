(define (iter-improve good? improve)
  (lambda (guess)
    (iter-it good? improve guess)))

(define (iter-it good? improve guess)
  (if (good? guess)
      guess
      (iter-it good? improve (improve guess))))

(define (sqrt1 x)
  ((iter-improve
    (lambda (g) (< (abs (- x (* g g))) 0.0001))
    (avg-damp (lambda (y) (/ x y))))
   1.0))


(define (fix f g t)
  ((iter-improve
    (lambda (x) (< (abs (- x (f x))) t))
    (lambda (x) (f x))) g))

(define (avg-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))
 
(define (sqrt2 x)
  (fix (avg-damp (lambda (y) (/ x y)))
       1.0
       0.00001))
          
(sqrt1 2)           
(sqrt2 2)