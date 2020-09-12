(define (acc op init seq)
  (if (null? seq)
      init
      (op (car seq) (acc op init (cdr seq)))))

(define (map f seq)
  (acc
    (lambda (x y) (cons (f x) y))
    '()
    seq))

(define (append a b)
  (acc cons b a))

(define (length seq)
  (acc (lambda (x y) (+ y 1)) 0 seq))


(define a '(1 '(2 '(3 4) 5) '(6 7)))
(define b '(0 1 2 3 4 5 6 7 8 9 10))
(define c '(1 2 3 4 5))

(map (lambda (x) (* x x)) b)
(append b c)
(length c)
 
