#lang sicp

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

(define (math? x)
  (cond
    ((eq? x 'add) #t)
    ((eq? x 'sub) #t)
    ((eq? x 'mul) #t)
    ((eq? x 'div) #t)
    (else #f)))

(define (drop x)
  (if (get 'project (list (type-tag x)))
      (let ((pushed (project x)))
        (if (equ? (raise pushed) x)
            (drop pushed)
            x))
      x))

;---

(define table '())

(define (put op type item)
  (set! table (cons (cons op (list type item)) table)))
  
(define (get op type)
  (define (get-from-table t)
    (if (eq? t '())
        #f
        (if (and (equal? (caar t) op)
                 (equal? (cadar t) type))
            (caddar t)
            (get-from-table (cdr t)))))
  (get-from-table table))

(define (apply-generic1 op . args)
  (apply (get op (map type-tag args)) (map contents args)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (if (math? op)
            (drop (apply proc (map contents args)))
            (apply proc (map contents args)))
        (let ((max-level (apply max (map tower-level type-tags)))
              (min-level (apply min (map tower-level type-tags))))
          (if (equal? max-level min-level)
              (error "No op found 1" (list op type-tags))
              (apply apply-generic (cons op (map (lambda (x) (raise-to max-level x)) args)))))))))


;---

(define tower '())

(define (tower-set type level)
  (set! tower (cons (list type level) tower)))
  
(define (tower-level type)
  (define (get-from-tower t)
    (if (eq? t '())
        #f
        (if (equal? (caar t) type)
            (cadar t)
            (get-from-tower (cdr t)))))
  (get-from-tower tower))

(define (raise-to level arg)
  (if (<= level (tower-level (type-tag arg)))
      arg
      (raise arg)))

;---

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum) (car datum) (error "Bad tagged datum" datum)))

(define (contents datum)
  (if (pair? datum) (cdr datum) (error "Bad tagged datum" datum)))

;---

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (quotient x y))))
  (put 'equ? '(integer integer) (lambda (x y) (tag (= x y))))
  (put '=zero? '(integer) (lambda (x) (tag (= x 0))))
  (put 'make '(integer) (lambda (x) (tag x)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))

  (tower-set 'integer 1)
  
  'done)

(define (make-integer x)
  ((get 'make '(integer)) x))


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
  (put 'make '(rational) (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational) (lambda (x) (make-real (/ (numer x) (denom x)))))
  (put 'project '(rational) (lambda (x) (make-integer (round (/ (numer x) (denom x))))))

  (tower-set 'rational (+ (tower-level 'integer) 1))
  
  'done)

(define (make-rational n d)
  ((get 'make '(rational)) n d))

;---

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real) (lambda (x y) (tag (= x y))))
  (put '=zero? '(real) (lambda (x) (tag (= x 0))))
  (put 'make '(real) (lambda (x) (tag x)))
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(real) (lambda (x) (make-rational (round (* x 1000000)) 1000000)))

  (tower-set 'real (+ (tower-level 'rational) 1))
  
  'done)

(define (make-real x)
  ((get 'make '(real)) x))

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
                ((get 'make-from-real-imag '(rectangular)) x y)))
  (define (make-from-mag-ang r a)
    (attach-tag 'polar
                ((get 'make-from-mag-ang '(polar)) r a)))

  
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex) (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex) (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex) (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex) (lambda (x y) (tag (div-complex x y))))
  (put 'equ? '(complex complex) (lambda (x y) (equ-complex? x y)))
  (put '=zero? '(complex) (lambda (x) (=zero-complex? x)))
  (put 'make-from-real-imag '(complex) (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(complex) (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'project '(complex) (lambda (x) (make-real (real-part x))))


  (tower-set 'complex (+ (tower-level 'real) 1))

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag '(complex)) x y))

(define (make-complex-from-mag-ang x y)
  ((get 'make-from-mag-ang '(complex)) x y))

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

  (put 'make-from-real-imag '(rectangular) make-from-real-imag)
  (put 'make-from-mag-ang '(rectangular) make-from-mag-ang)
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

  (put 'make-from-real-imag '(polar) make-from-real-imag)
  (put 'make-from-mag-ang '(polar) make-from-mag-ang)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  'done)

;---

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


"---"
(define a (make-integer 4))
(define b (raise a))
(define c (raise b))
(define d (raise c))
;(define e (raise d)) ; error
a
b
c
d
"---"
(drop a)
(drop b)
(drop c)
(drop d)
(drop (make-complex-from-real-imag 3.25 0.0))
(add d d)
(add a d)

; (real-part a) - special operators still not common, so additional work required to repair this
