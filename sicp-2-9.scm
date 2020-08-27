(define (make-interval a b)
  (list a b))

(define (lower x)
  (first x))

(define (upper x)
  (second x))

(define (width x)
  (/ (- (upper x) (lower x)) 2))

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

(define (sub x y)
  (add
    x
    (make-interval
      (- (upper y))
      (- (lower y)))))

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


(define a (make-interval 2 6))
(define b (make-interval 3 9))
(define c (make-interval 8 12))

(print-interval a)
(print-interval b)
(print-interval c)

"---"

(width a)
(width b)
(width c)

"---"

(width (add a b))
(width (add c b))
(width (sub a b))
(width (sub c b))
(width (mul a b))
(width (mul c b))
(width (div a b))
(width (div c b))