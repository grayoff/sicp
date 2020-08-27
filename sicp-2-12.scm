(define (make-interval a b)
  (list a b))

(define (lower x)
  (first x))

(define (upper x)
  (second x))

(define (width x)
  (/ (- (upper x) (lower x)) 2.0))

(define (make-c-w c w)
  (make-interval (- c w) (+ c w)))

(define (center2 x)
  (+ (lower x) (width x)))

(define (center x)
  (/ (+ (lower x) (upper x)) 2.0))

(define (make-c-t c t)
  (let ((w (abs (* c t))))
    (make-c-w c w)))

(define (tolerance x)
  (/ (width x) (center x)))

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
  (let ((ya (lower y))
        (yb (upper y)))
    (if (and (< ya 0) (> yb 0))
        (error "Division by zero span interval")
        (mul
          x
          (make-interval
            (/ 1.0 yb)
            (/ 1.0 ya))))))


(define a (make-interval 1.0 3.0))
(define b (make-c-w 2.0 1.0))
(define c (make-c-t 2.0 0.5))

(print-interval a)
(print-interval b)
(print-interval c)

"---"

(center a)
(center b)
(center c)
(width a)
(width b)
(width c)
(tolerance a)
(tolerance b)
(tolerance c)

