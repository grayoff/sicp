(define (acc op init seq)
  (if (null? seq)
      init
      (op (car seq) (acc op init (cdr seq)))))

(define (horner x coeffs)
  (acc
    (lambda (this-coef high-term)
      (+ (* high-term x) this-coef))
    0
    coeffs))


(let ((x 2))
  (+ 1 (* 3 x) (* 5 x x x) (* x x x x x)))

(horner 2 (list 1 3 0 5 0 1))
 
