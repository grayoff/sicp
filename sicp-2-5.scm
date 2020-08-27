(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (get-exp 2 x))

(define (cdr x)
  (get-exp 3 x))

(define (get-exp n x)
  (if (= (% x n) 0)
      (+ 1 (get-exp n (/ x n)))
      0))

(define a (cons 2 4))
(car a)
(cdr a)

(define a (cons 0 3))
(car a)
(cdr a)

(define a (cons 0 0))
(car a)
(cdr a)


