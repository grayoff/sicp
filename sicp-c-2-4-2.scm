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

(define (attach-tag type-tag contents)
 (list type-tag contents))

(define (type-tag datum)
 (if (cons? datum)
     (first datum)
     (error "Bad tagged datum" datum)))

(define (contents datum)
 (if (cons? datum)
     (second datum)
     (error "Bad tagged datum" datum)))

(define (rectangular? z)
 (symbol=? (type-tag z) 'rectangular))

(define (polar? z)
 (symbol=? (type-tag z) 'polar))

;---

(define (make-from-real-imag x y)
 (attach-tag 'rectangular 
             (make-from-real-imag-r x y)))

(define (make-from-mag-ang r a)
 (attach-tag 'polar
             (make-from-mag-ang-p r a)))

(define (real-part z)
 (cond
  ((rectangular? z)
   (real-part-r (contents z)))
  ((polar? z)
   (real-part-p (contents z)))
  (else (error "Unknown type" z))))
       

(define (imag-part z)
 (cond
  ((rectangular? z)
   (imag-part-r (contents z)))
  ((polar? z)
   (imag-part-p (contents z)))
  (else (error "Unknown type" z))))

(define (magnitude z)
 (cond
  ((rectangular? z)
   (magnitude-r (contents z)))
  ((polar? z)
   (magnitude-p (contents z)))
  (else (error "Unknown type" z))))

(define (angle z)
 (cond
  ((rectangular? z)
   (angle-r (contents z)))
  ((polar? z)
   (angle-p (contents z)))
  (else (error "Unknown type" z))))

;---

(define z1 (make-from-real-imag 3.0 4.0))
(define z2 (make-from-real-imag 2.0 -5.0))

z1
z2
"---"
(real-part (add-complex z1 z2))
(imag-part (add-complex z1 z2))
(real-part (sub-complex z1 z2))
(imag-part (sub-complex z1 z2))
(real-part (mul-complex z1 z2))
(imag-part (mul-complex z1 z2))
(real-part (div-complex z1 z2))
(imag-part (div-complex z1 z2))
"---"
(magnitude z1)
(angle z1)
(define z3 (make-from-mag-ang 5.0 0.9272952180016122))
(real-part z3)
(imag-part z3)
"---"
(real-part (add-complex z3 z2))
(imag-part (add-complex z3 z2))