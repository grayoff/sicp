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


(define (d x)
  (let ((y (+ x 1)))
    (if (= (% y 3) 0)
        (* (/ y 3) 2)
        1)))


(define (e)
  (+ 2
     (cont-frac
     (lambda (x) 1.0)
     d
     10)))

(e)


(d 1)
(d 2)
(d 3)
(d 4)
(d 5)
(d 6)
(d 7)
