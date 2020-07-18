(define (sqrt x)
  (let (
  (good-enough? (lambda (guess last-guess)
    (< (/ (abs (- guess last-guess)) guess) 0.001)))
  (sqrt-iter (lambda (guess last-guess) 
    (if (good-enough? guess last-guess)
        guess
        (sqrt-iter (better-guess guess) guess))))
  (better-guess (lambda (guess)
    (average guess (/ x guess)))))
  (sqrt-iter 1.0 1.8e308)))
(define (average a b)
  (/ (+ a b) 2))
(define (square x) (* x x))

(sqrt 0.0001)
(sqrt 0.4e47)
(sqrt 0.4e307)
(sqrt 9)
