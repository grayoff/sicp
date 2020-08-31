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

(define (print-c-t x)
  (string-append
    "["
    (number->string (round2 (center x)))
    ","
    (number->string (round2 (tolerance x)))
    "]"))

(define (par1 r1 r2)
  (div (mul r1 r2) (add r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div one
         (add (div one r1)
              (div one r2)))))


(define a (make-c-t 8.0 0.1))
(define b (make-c-t 6.0 0.01))
(define c (make-c-t 7.0 0.01))
(define d (make-c-t 2.0 0.2))

;(print-c-t a)
(print-c-t b)
(print-c-t c)
;(print-c-t d)

"---"
(define p1 (par1 b c))
(define p2 (par2 b c))

(print-interval p1)
(print-interval p2)
(print-c-t p1)
(print-c-t p2)

"---"
(print-c-t (div a a))
(print-c-t (div a d))