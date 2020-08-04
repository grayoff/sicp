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

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

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

(integral cube 0.0 1.0 0.001)
