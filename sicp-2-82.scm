#lang sicp

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (sum x y z) (apply-generic 'sum x y z))

;---

(define table-coercion '())

(define (put-coercion type1 type2 item)
  (set! table-coercion (cons (cons type1 (list type2 item)) table-coercion)))
  
(define (get-coercion type1 type2)
  (define (get-from-table t)
    (if (eq? t '())
        #f
        (if (and (equal? (caar t) type1)
                 (equal? (cadar t) type2))
            (caddar t)
            (get-from-table (cdr t)))))
  (if (equal? type1 type2)
      identity
      (get-from-table table-coercion)))

;---

(define table '())

(define (put op type item)
  (set! table (cons (cons op (list type item)) table)))
  
(define (get op type)
  (define (get-from-table t)
    (if (eq? t '())
        #f
        (if (and (eq? (caar t) op)
                 (equal? (cadar t) type))
            (caddar t)
            (get-from-table (cdr t)))))
  (get-from-table table))

(define (apply-generic op . args)
  (define (try-coercion types type-tags)
    (if (eq? types '())
        (error "No op found 2" (list op type-tags))
        (let ((type1 (car types)))
          (let ((proc (get op (map (lambda (x) type1) type-tags))))
            (if proc
                (let ((variant (map (lambda (type2) (get-coercion type2 type1)) type-tags)))
                  (if (all variant)
                      (apply proc (map (lambda (pair) (contents ((car pair) (cdr pair)))) (zip variant args)))
                      (try-coercion (cdr types) type-tags)))
                (try-coercion (cdr types) type-tags))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-coercion (list->set type-tags) type-tags)))))

;---

(define (acc op init seq)
  (if (null? seq)
      init
      (op (car seq) (acc op init (cdr seq)))))

(define (filter pred seq)
  (if (null? seq)
      seq
      (let ((a (car seq)))
        (if (pred a)
            (cons a (filter pred (cdr seq)))
            (filter pred (cdr seq))))))
    
(define (flatmap proc seq)
  (acc append '() (map proc seq)))

(define (all xs)
  (acc (lambda (x res) (and x res)) #t xs))

(define (zip seq1 seq2)
  (cond ((null? seq1) '())
        ((null? seq2) '())
        (else (cons (cons (car seq1) (car seq2)) (zip (cdr seq1) (cdr seq2))))))


;---

(define (element-of-set? x s)
  (if (null? s)
      #f
      (if (equal? x (car s))
          #t
          (element-of-set? x (cdr s)))))

(define (adjoin-set x s)
  (if (element-of-set? x s)
      s
      (cons x s)))

(define (list->set xs)
  (acc adjoin-set '() xs))

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
  (put 'exp '(scheme-number scheme-number) expt)
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

  (define (sum-complex z1 z2 z3)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2) (real-part z3))
     (+ (imag-part z1) (imag-part z2) (imag-part z3))))

  
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
  (put 'sum '(complex complex complex) (lambda (x y z) (tag (sum-complex x y z))))

  (define (scheme-number->complex n)
    (tag (make-from-real-imag (contents n) 0)))

  (put-coercion 'scheme-number 'complex scheme-number->complex)
  
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


"---"
(define z1 (make-complex-from-real-imag 3.0 4.0))
(sum z1 z1 z1)
(sum z1 2 2)
(sum 2 2 z1)
(sum 2 z1 2)
(sum z1 z1 2)
(sum z1 2 z1)
(sum 2 z1 z1)

; Not sufficient:
; a) when there is (complex, complex, complex) and (int->complex) then (int, int, int) not found
; b) when there is (complex, complex, int) and (int->complex) then (int, complex, int) not found


;(get op type-tags)
;(define type-tags (list 'scheme-number 'scheme-number 'complex))
;(define op 'sum)
;(define args (list 2 2 z1)) 
;(list->set type-tags)
;(define (try-coercion types type-tags)
;  (if (eq? types '())
;      (error "No op found 4" (list op type-tags))
;      (let ((type1 (car types)))
;        (let ((proc (get op (map (lambda (x) type1) type-tags))))
;          (if proc
;              (let ((variant (map (lambda (type2) (get-coercion type2 type1)) type-tags)))
;                (if (all variant)
;                    ;variant
;                    ;(zip variant args)
;                    ;(map (lambda (pair) ((car pair) (cdr pair))) (zip variant args))
;                    ;(apply proc (map (lambda (pair) (contents ((car pair) (cdr pair)))) (zip variant args)))
;                    (try-coercion (cdr types) type-tags)))
;              (try-coercion (cdr types) type-tags))))))
;         
;(get op (map (lambda (x) 'complex) type-tags))
;(map (lambda (type2) (get-coercion 'complex type2)) type-tags)
;(try-coercion (list->set type-tags) type-tags)
;(map (lambda (x) 'scheme-number) type-tags)