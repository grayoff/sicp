(define (root1 f a b t)
  (let ((c (/ (+ a b) 2)))
    (cond ((<= (- b a) t) c)
          ((> (f c) 0) (root1 f a c t))
          (else (root1 f c b t)))))

(define (root f a b t)
  (let ((c (/ (+ a b) 2)))
    (if (<= (- b a) t) c
       (let ((r (f c)))
         (cond ((> r 0) (root f a c t))
               ((< r 0) (root f c b t))
               (else c))))))

(root (lambda (x) (- (* x x) 4)) -1.0 7.0 0.001)
