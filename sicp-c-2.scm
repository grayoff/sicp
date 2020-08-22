(define (vec a b)
  (lambda (n)
    (cond ((= n 0) a)
          ((= n 1) b)
          (else (error "Out of range")))))

(define (frst v)
  (v 0))

(define (scnd v)
  (v 1))

(define (add v1 v2)
  (vec (+ (frst v1) (frst v2))
       (+ (scnd v1) (scnd v2))))

(define x (vec 1 2))
(define y (vec 3 4))
(define z (add x y))

(frst z)
(scnd z)