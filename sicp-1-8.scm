(define (cuberoot-iter guess last-guess x) 
  (if (good-enough? guess last-guess)
      guess
      (cuberoot-iter (better-guess guess x) guess x)))
(define (good-enough? guess last-guess)
  (< (/ (abs (- guess last-guess)) guess) 0.001))
(define (better-guess guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))
(define (square x) (* x x))
(define (cuberoot x) (cuberoot-iter 1.0 1.8e308 x))
(square 2)
(better-guess 1.0 2)
(good-enough? 1.4142 1.4143)
(good-enough? 1.5 2)
(cuberoot 27)
(cuberoot 64)