(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (+ a 1) b))))

(define (cube x) (* x x x))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(define (sum2 term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum term a next b)
  (sum-iter term a next b 0))
(define (sum-iter term a next b result)
  (if (> a b) result
      (sum-iter term (next a) next b (+ (term a) result))))

(define (id x) x)

(define (inc x) (+ x 1))

(define (sum-int2 a b)
  (sum id a inc b))

(define (sum-cubes2 a b)
  (sum cube a inc b))

(define (pi-sum2 a b)
  (sum
    (lambda (x) (/ 1.0 (* x (+ x 2))))
    a
    (lambda (x) (+ x 4))
    b))

(define (integral f a b dx)
  (* (sum
      f
      (+ a (/ dx 2))
      (lambda (x) (+ x dx))
      b)
     dx))

(define (simpson f a b n)
  (* (/ (h a b n) 3)
     (sum
       (lambda (x)
        (cond ((= x 0) (y x f a b n))
              ((= x n) (y x f a b n))
              ((even? x) (* 2 (y x f a b n)))
              (else (* 4 (y x f a b n)))))
       0
       inc
       n))) 
(define (h a b n) (/ (- b a) n))
(define (y x f a b n) (f (+ a (* x (h a b n)))))

(sum-int 3 5)
(sum-int 1 3)
(sum-int 1 1000)
(sum-cubes 3 5)
(* 8 (pi-sum 1 1000))

((lambda (x) (* x x)) 3)

(sum-int2 3 5)
(sum-int2 1 3)
(sum-int2 1 1000)
(sum-cubes2 3 5)
(* 8 (pi-sum2 1 1000))

(integral cube 0.0 1.0 0.01)
(integral cube 0.0 1.0 0.001)
(simpson cube 0.0 1.0 100)
(simpson cube 0.0 1.0 1000)

