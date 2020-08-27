(define zero
  (lambda (f)
    (lambda (x) x)))

(define (succ n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

(define (inc x) (+ x 1))

(define (church->number n)
  ((n inc) 0))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define (add a b)
  (lambda (f)
    (lambda (x) ((a f) ((b f) x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (mul a b)
  (lambda (f)
    (lambda (x)
      (((compose a b) f) x))))

(church->number zero)
(church->number (succ zero))
(church->number one)

(define three (succ (succ one)))
(define four (succ three))

(church->number (add three four))
(church->number (add three one))
(church->number (add zero four))
(church->number (add zero zero))

(church->number (mul three four))
(church->number (mul zero zero))
(church->number (mul three one))
(church->number (mul three zero))

