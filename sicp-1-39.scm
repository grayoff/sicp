(define (cont-frac n d k)
  (k-term n d k 1))

(define (k-term n d k i)
  (/ (n i)
     (if (= i k)
       (d i)
       (+ (d i)
          (k-term n d k (+ i 1))))))

(define (cont-frac2 n d k)
  (k-term2 n d k k 0))

(define (k-term2 n d k i r)
  (if (= i 0)
      r
      (k-term2 n d k (- i 1) (/ (n i) (+(d i) r)))))


(define (tan2 x k)
  (cont-frac
     (lambda (i)
       (if (= i 1)
           x
           (- (* x x))))
     (lambda (i) (- (* i 2.0) 1.0))
     k))

(tan2 3.14 10)
(tan 3.14) 
