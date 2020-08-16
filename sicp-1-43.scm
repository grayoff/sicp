(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (even? x) (= (% x 2) 0))

(define (id x) x)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (double f)
  (lambda (x) (f (f x))))

(define (repeated1 f n)
  (cond ((= n 0) id)
        (else (compose f (repeated f (- n 1))))))

(define (repeated f n)
  (cond ((= n 0) id)
        ((even? n) (double (repeated f (/ n 2))))
        (else (compose f (repeated f (- n 1))))))

(define (acc comb nul term a next b)
  (if (> a b)
      nul
      (comb (term a)
            (acc comb nul term (next a) next b))))

(define (repeated2 f n)
  (acc compose id (lambda (x) f) 1 inc n))
       

((repeated square 2) 5)
((repeated inc 1000) 0)

((repeated2 square 2) 5)
((repeated2 inc 1000) 0)

