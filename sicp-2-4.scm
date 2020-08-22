(define (cons a b)
  (lambda (m) (m a b)))

(define (car x)
  (x (lambda (a b) a)))

(define (cdr x)
  (x (lambda (a b) b)))


(define a (cons 3 5))
(car a)
(cdr a)
