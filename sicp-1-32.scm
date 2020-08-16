(define (id x) x)

(define (inc x) (+ x 1))
       
(define (acc comb null term a next b)
  (if (> a b) null
      (comb (term a)
            (acc comb null term (next a) next b))))

(define (acc2 comb null term a next b)
  (acc-it comb term a next b null))
(define (acc-it comb term a next b res)
  (if (> a b) res
      (acc-it comb term (next a) next b (comb a res))))
   
(define (sum term a next b)
  (acc2 + 0 term a next b))

(define (prod term a next b)
  (acc2 * 1 term a next b))

(sum id 1 inc 5)
(prod id 1 inc 5)
