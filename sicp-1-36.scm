(define (fix f x t)
  (let ((y (f x)))
   (begin (println y)
    (if (<= (abs (- y x)) t)
        y
        (fix f y t)))))
  
(define (average x y)
  (/ (+ x y) 2))


(fix (lambda (x) (/ (log 1000) (log x)))
     2.0    
     0.00001)

"damp"   

(fix (lambda (x) (average x (/ (log 1000) (log x))))
     2.0    
     0.00001)
