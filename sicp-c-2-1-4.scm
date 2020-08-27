(define (make-interval a b)
  (list a b))

(define (lower x)
  (first x))

(define (upper x)
  (second x))

(define (print-interval x)
  (string-append
    "["
    (number->string (lower x))
    ","
    (number->string (upper x))
    "]"))

(define (add x y)
  (make-interval
    (+ (lower x) (lower y))
    (+ (upper x) (upper y))))

(define (mul x y)
  (let ((p1 (* (lower x) (lower y)))
        (p2 (* (lower x) (upper y)))
        (p3 (* (upper x) (lower y)))
        (p4 (* (upper x) (upper y))))
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div x y)
  (mul
    x
    (make-interval
      (/ 1.0 (upper y))
      (/ 1.0 (lower y)))))


(define a (make-interval 3 6))
(define b (make-interval -5 -2))

(print-interval a)
(print-interval b)

(print-interval (add a b))
(print-interval (mul a b))
(print-interval (div a b))