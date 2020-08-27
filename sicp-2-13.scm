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

(define (print-interval x)
  (string-append
    "["
    (number->string (lower x))
    ","
    (number->string (upper x))
    "]"))

(define a (make-c-t 2.0 0.0))
(define b (make-c-t 6.0 0.2))
(define c (make-c-t 7.0 0.1))
(define d (make-c-t 9.0 0.2))

(print-interval a)
(print-interval b)
(print-interval c)
(print-interval d)

"---"

(tolerance (mul b c))
(tolerance (mul c d))

(define (round2 x)
  (/ (round (* x 100)) 100.0))

(define (iter a b w)
  (if (> a b) ""
  (begin
    (print "(")
    (print (round2 a))
    (print ",")
    (print (round2 (tolerance
            (mul (make-c-t 20.0 a)
                  (make-c-t 40.0 w)))))
    (print "),")
    (iter (+ a 0.1) b w))))

;(iter 0.0 1.0 0.3)

; t = f t0 t1
; a = a0 * a1
; b = b0 * b1
; t = (b - a) / (2 * (b + a) / 2)
; t = (b - a) / (b + a)
(/ (- 7.2 4.8) (+ 7.2 4.8))
; с = (b + a) / 2
; c = c0 * c1
; c * (1 - t) = a
; c * (1 + t) = b
; c = (a + b) / 2

;t = (b0 * b1 - a0 * a1) / (b0 * b1 + a0 * a1)

;t = (c0 * c1 *((1 + t0)(1 + t1) - (1 - t0)(1 - t1))) / (c0 * c1 *((1 + t0)(1 + t1) + (1 - t0)(1 - t1)))

;t = ((1 + t0)(1 + t1) - (1 - t0)(1 - t1)) / ((1 + t0)(1 + t1) + (1 - t0)(1 - t1))

;t = (t0 + t1)/(t0 t1 + 1)