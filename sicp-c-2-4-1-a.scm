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


(define (square x)
 (* x x))

;---

(define (make-from-real-imag-r x y)
 (list x y))

(define (make-from-mag-ang-r r a)
 (list (* r (cos a)) (* r (sin a))))

(define (real-part-r z)
 (first z))

(define (imag-part-r z)
 (second z))

(define (magnitude-r z)
 (sqrt (+ (square (real-part-r z))
          (square (imag-part-r z)))))

(define (angle-r z)
 (atan (/ (imag-part-r z) (real-part-r z))))

;---

(define (make-from-real-imag-p x y)
 (list (sqrt (+ (square x) (square y)))
       (atan (/ y x))))

(define (make-from-mag-ang-p r a)
 (list r a))

(define (real-part-p z)
 (* (magnitude-p z)
    (cos (angle-p z))))

(define (imag-part-p z)
 (* (magnitude-p z)
    (sin (angle-p z))))

(define (magnitude-p z)
 (first z))

(define (angle-p z)
 (second z))

;---

(define make-from-real-imag make-from-real-imag-p)

(define make-from-mag-ang make-from-mag-ang-p)

(define real-part real-part-p)

(define imag-part imag-part-p)

(define magnitude magnitude-p)

(define angle angle-p)

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
