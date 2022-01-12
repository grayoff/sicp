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

(define table '())

(define (put op type item)
  (set! table (cons (cons op (list type item)) table)))
  
(define (get op type)
  (define (get-from-table t)
    (if (eq? t '())
        (error "No op found" op type)
        (if (and (eq? (caar t) op)
                 (equal? (cadar t) type))
            (caddar t)
            (get-from-table (cdr t)))))
  (get-from-table table))

(define (apply-generic op . args)
  (apply (get op (map type-tag args)) (map contents args)))
  
;---

(define (attach-tag type-tag contents)
 (list type-tag contents))

(define (type-tag datum)
 (if (pair? datum)
     (car datum)
     (error "Bad tagged datum" datum)))

(define (contents datum)
 (if (pair? datum)
     (cadr datum)
     (error "Bad tagged datum" datum)))

(define (rectangular? z)
 (eq? (type-tag z) 'rectangular))

(define (polar? z)
 (eq? (type-tag z) 'polar))

;---

(define (make-from-real-imag x y)
 (attach-tag 'rectangular 
             ((get 'make-from-real-imag 'rectangular) x y)))

(define (make-from-mag-ang r a)
 (attach-tag 'polar
             ((get 'make-from-mag-ang 'polar) r a)))

(define (real-part z)
  (apply-generic 'real-part z))

(define (imag-part z)
  (apply-generic 'imag-part z))

(define (magnitude z)
  (apply-generic 'magnitude z))

(define (angle z)
  (apply-generic 'angle z))

;---

(define (install-rectangular-package)
  (define (make-from-real-imag x y)
    (list x y))

  (define (make-from-mag-ang r a)
    (list (* r (cos a)) (* r (sin a))))

  (define (real-part z)
    (car z))

  (define (imag-part z)
    (cadr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (angle z)
    (atan (/ (imag-part z) (real-part z))))

  (put 'make-from-real-imag 'rectangular make-from-real-imag)
  (put 'make-from-mag-ang 'rectangular make-from-mag-ang)
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle))

;---

(define (install-polar-package)
  (define (make-from-real-imag x y)
    (list (sqrt (+ (square x) (square y)))
          (atan (/ y x))))

  (define (make-from-mag-ang r a)
    (list r a))

  (define (real-part z)
    (* (magnitude z)
       (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z)
       (sin (angle z))))

  (define (magnitude z)
    (car z))

  (define (angle z)
    (cadr z))

  (put 'make-from-real-imag 'polar make-from-real-imag)
  (put 'make-from-mag-ang 'polar make-from-mag-ang)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle))


;---
(install-rectangular-package)
(install-polar-package)

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