#lang sicp

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

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
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum" datum))))

;---

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  'done)

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
  (define (equ-rat? x y)
    (and
     (= (numer x) (numer y))
     (= (denom x) (denom y))))
  (define (=zero-rat? x)
    (= (numer x) 0))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) (lambda (x y) (equ-rat? x y)))
  (put '=zero? '(rational) (lambda (x) (=zero-rat? x)))
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
  (define (equ-complex? z1 z2)
    (and
     (= (real-part z1) (real-part z2))
     (= (imag-part z1) (imag-part z2))))
  (define (=zero-complex? z1)
    (equ-complex? z1 (make-from-real-imag 0 0)))
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
  (put 'equ? '(complex complex) (lambda (x y) (equ-complex? x y)))
  (put '=zero? '(complex) (lambda (x) (=zero-complex? x)))
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


(define i1 3.0)
(define i2 4.0)
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
(angle z1)

"---"
(equ? 3 4)
(equ? 5 5)
(equ? a b)
(equ? a (make-rational 4 8))
(equ? z1 z2)
(equ? z1 (make-complex-from-mag-ang 5.0 0.9272952180016123))

"---"
(=zero? 5)
(=zero? 0)
(=zero? a)
(=zero? (make-rational 0 5))
(=zero? z1)
(=zero? (make-complex-from-mag-ang 0.0 0.9272952180016123))
