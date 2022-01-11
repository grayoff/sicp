(define (add-complex z1 z2)
 (make-from-real-imag
  (+ (real-part z1) (real-part z2))
  (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
 (make-from-real-imag
  (- (real-part z1) (real-part z2))
  (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
 (make-from-mag-ang
  (* (magnitude z1) (magnitude z2))
  (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
 (make-from-mag-ang
  (/ (magnitude z1) (magnitude z2))
  (- (angle z1) (angle z2))))


(define (make-from-real-imag x y)
 (list x y))

(define (make-from-mag-ang r a)
 (list (* r (cos a)) (* r (sin a))))

(define (real-part z)
 (first z))

(define (imag-part z)
 (second z))

(define (magnitude z)
 (sqrt (+ (square (real-part z))
          (square (imag-part z)))))

(define (angle z)
 (atan (/ (imag-part z) (real-part z))))

(define (square x)
 (* x x))

;---

(define z1 (make-from-real-imag 3.0 4.0))
(define z2 (make-from-real-imag 2.0 -5.0))

z1
z2
"---"
(add-complex z1 z2)
(sub-complex z1 z2)
(mul-complex z1 z2)
(div-complex z1 z2)
"---"
(magnitude z1)
(angle z1)
(define z3 (make-from-mag-ang 5.0 0.92730))
z3
"---"
