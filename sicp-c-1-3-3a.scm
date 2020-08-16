(define (fix f x t)
  (let ((y (f x)))
    (if (<= (abs (- y x)) t)
        y
        (fix f y t))))

(define (sqrt x)
  (fix (lambda (y) (/ (+ y (/ x y)) 2)) 1.0 0.00001))
  

(fix cos 1.0 0.00001)
(fix (lambda (x) (+ (sin x) (cos x))) 1.0 0.00001)

(fix sin 1.0 0.00001)
(fix tan 1.0 0.00001) 

(sqrt 2.0)   

