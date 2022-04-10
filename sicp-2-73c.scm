#lang sicp

(define (deriv e v)
  (cond
    ((number? e) 0)
    ((variable? e)
      (if (same-variable? e v) 1 0))
    (else ((get 'deriv (operator e)) (operands e) v))))
    

(define operator car)
(define operands cdr)

(define (number=? a b)
  (and (number? a) (= a b)))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

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

;---

(define (install-sum-package)
  (define (deriv-sum o v)
    (make-sum (deriv (addend o) v)
              (deriv (augend o) v)))

  (define (make-sum a b)
    (cond
      ((number=? a 0) b)
      ((number=? b 0) a)
      ((and (number? a) (number? b))
       (+ a b))
      (else (list '+ a b))))

  (define addend car)

  (define augend cadr)

  (put 'deriv '+ deriv-sum)
  (put 'make-sum 'constructor make-sum))


(define (install-product-package)
  (define make-sum (get 'make-sum 'constructor))
  
  (define (deriv-product o v)
    (make-sum
     (make-product
      (multiplier o)
      (deriv (multiplicand o) v))
     (make-product
      (multiplicand o)
      (deriv (multiplier o) v))))

  (define (make-product a b)
    (cond
      ((or (number=? a 0) (number=? b 0))
       0)
      ((number=? a 1) b)
      ((number=? b 1) a)
      ((and (number? a) (number? b))
       (* a b))
      (else (list '* a b))))

  (define multiplier car)

  (define multiplicand cadr)

  (put 'deriv '* deriv-product)
  (put 'make-product 'constructor make-product))


(define (install-expt-package)
  (define make-product (get 'make-product 'constructor))
  
  (define (deriv-expt o v)
    (let ((b (base o))
          (p (power o)))
      (make-product
       (make-product
        p
        (make-expt b (- p 1)))
       (deriv b v))))

  (define (make-expt a b)
    (cond
      ((number=? b 0) 1)
      ((number=? b 1) a)
      ;((number=? a 0) 0)
      ;((number=? a 1) 1)
      ((and (number? a) (number? b))
       (expt a b))
      (else (list 'expt a b))))

  (define base car)

  (define power cadr)       

  (put 'deriv 'expt deriv-expt)
  (put 'make-expt 'constructor make-expt))


;---
(install-sum-package)
(install-product-package)
(install-expt-package)

(deriv '(+ 3 x) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(+ (expt x 2) (* x 2)) 'x)

; 1
; y
; (+ (* x y) (* (+ x 3) y))
; (+ (* 2 x) 2)

; a
; number? and variable? can't be included into dispatch because
; dispatch only works on tagged data (type + datum), not primitive 
