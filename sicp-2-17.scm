(define (last a)
  (let ((b (cdr a)))
    (if (empty? b)
        (car a)
        (last b))))

(define a (list 1 2 3 4))
(define sq (list 0 1 4 9 16 25))

(last a)
(last sq)