(define (id x) x)

(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (even? x) (= (% x 2) 0))

(define (gcd a b)
  (if (= b 0) a
              (gcd b (% a b))))

(define (prime? n)
  (= (sm-div n) n))
(define (sm-div n)
  (find-div n 2))
(define (find-div n t)
  (cond ((> (square t) n) n)
        ((= (% n t) 0) t)
        (else (find-div n (+ t 1)))))
       
(define (f-acc comb null pred term a next b)
  (if (> a b) null
      (if (pred a)
           (comb (term a)
                 (f-acc comb null pred term (next a) next b))
           (f-acc comb null pred term (next a) next b))))
   

(f-acc + 0 even? id 1 inc 5)
(f-acc + 0 prime? square 1 inc 9)
(f-acc * 1
  (lambda (x) (= (gcd 20 x) 1))
  id 17 inc 20)


