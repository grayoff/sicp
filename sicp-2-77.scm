#lang sicp

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

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

;---

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;---

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (reduced f)
    (lambda (a b)
      (let ((g (gcd a b)))
        (f (/ a g) (/ b g)))))
  (define (norm-sign f)
    (lambda (a b)
      (if (> b 0)
          (f a b)
          (f (- a) (- b)))))
  (define (make-rat n d)
    ((reduced (norm-sign cons)) n d))
  (define (add-rat x y)
    (make-rat
     (+ (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat
     (- (* (numer x) (denom y))
        (* (numer y) (denom x)))
     (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat
     (* (numer x) (numer y))
     (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat
     (* (numer x) (denom y))
     (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;---

(define (install-complex-package)
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
    (attach-tag 'rectangular 
                ((get 'make-from-real-imag 'rectangular) x y)))
  (define (make-from-mag-ang r a)
    (attach-tag 'polar
                ((get 'make-from-mag-ang 'polar) r a)))

  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex) (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex) (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex) (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex) (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))

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
  (define (square x) (* x x))
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
  (put 'angle '(rectangular) angle)
  'done)

;---

(define (install-polar-package)
  (define (square x) (* x x))
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
  (put 'angle '(polar) angle)
  'done)

;---

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


(define i1 (make-scheme-number 3.0))
(define i2 (make-scheme-number 4.0))
i1
i2
(add i1 i2)
(sub i1 i2)
(mul i1 i2)
(div i1 i2)
"---"
(define a (make-rational 1 2))
(define b (make-rational 1 3))
a
b
(add a b)
(sub a b)
(mul a b)
(div a b)
"---"
(define z1 (make-complex-from-real-imag 3.0 4.0))
(define z2 (make-complex-from-real-imag 2.0 -5.0))
z1
z2
(add z1 z2)
(sub z1 z2)
(mul z1 z2)
(div z1 z2)
"---"
(magnitude z1)
; (magnitude z1)                                             ; (complex (rectangular (3.0 4.0)))
; (apply-generic 'magnitude z1))                             ; (complex (rectangular (3.0 4.0))) // 1
; (apply (get op (map type-tag args)) (map contents args)))  
; (apply (get 'magnitude 'complex) (rectangular (3.0 4.0))))
; (magnitude z)                                              ; (rectangular (3.0 4.0))
; (apply-generic 'magnitude z))                              ; (rectangular (3.0 4.0)) // 2
; (apply (get op (map type-tag args)) (map contents args)))  
; (apply (get 'magnitude 'rectangular) (3.0 4.0)))
; (sqrt (+ (square (real-part z))(square (imag-part z))))
; 5.0