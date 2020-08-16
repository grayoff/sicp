(define (fix f x t)
  (let ((y (f x)))
    (if (<= (abs (- y x)) t)
        y
        (fix f y t))))
  

(fix (lambda (x) (+ 1 (/ 1 x)))
     1.0    
     0.00001)   

(fix (lambda (x) (- (/ 1 x) 1))
     1.0    
     0.00001)   
