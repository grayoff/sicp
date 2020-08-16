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

(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           11)

(cont-frac2 (lambda (x) 1.0)
           (lambda (x) 1.0)
           11)