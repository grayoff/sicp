#lang sicp

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

;---

(define (square x)
 (* x x))

;---

(define (apply-generic op arg)
  (arg op))
  
;---

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle)
           (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)
  
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))


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
z3


;(rectangular (3.0 4.0))
;(rectangular (2.0 -5.0))
;"---"
;5.0
;-1.0
;1.0
;9.0
;26.0
;-6.999999999999999
;-0.4827586206896552
;0.7931034482758622
;"---"
;5.0
;0.9272952180016122
;3.0000000000000004
;3.9999999999999996
;"---"
;5.0
;-1.0000000000000004
;(polar (5.0 0.9272952180016122))