(define (product term a next b)
  (if (> a b) 1
      (* (term a)
         (product term (next a) next b))))

(define (id x) x)

(define (inc x) (+ x 1))

(define (fac n)
  (product id 2 inc n))

(define (product2 term a next b)
  (prod-it term a next b 1))
(define (prod-it term a next b result)
  (if (> a b) result
      (prod-it term (next a) next b (* (term a) result)))) 

(define (fac2 n)
  (product2 id 2 inc n))

(define (pi n)
  (* 4.0
     (product2
       (lambda (x) (/ (* x (+ x 2.0)) (square (+ x 1.0))))
       2.0
       (lambda (x) (+ x 2.0))
       n)))

(define (square x) (* x x))
       


(fac 1)
(fac 2)
(fac 3)
(fac 4)
(fac 5)

(fac2 1)
(fac2 2)
(fac2 3)
(fac2 4)
(fac2 5)

(pi 100)
(pi 1000)
