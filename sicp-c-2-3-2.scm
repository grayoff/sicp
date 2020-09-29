(define (eq? a b)
  (cond
    ((and (symbol? a) (symbol? b))
      (symbol=? a b))
    ((and (string? a) (string? b))
      (string=? a b))
    ((and (number? a) (number? b))
      (= a b))
    ((and (char? a) (char? b))
      (char=? a b)) 
    (else #f)))

(define (memq item x)
  (if (null? x)
      false
      (if (eq? item (car x))
          x
          (memq item (cdr x)))))

(define (equal? a b)
  (if (and (list? a) (list? b))
      (cond
        ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        (else
          (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b)))))    
      (eq? a b)))

(define (number=? a b)
  (and (number? a) (= a b)))

(define (deriv e v)
  (cond
    ((number? e) 0)
    ((variable? e)
      (if (same-variable? e v) 1 0))
    ((sum? e)
      (make-sum (deriv (addend e) v)
                (deriv (augend e) v)))
    ((product? e)
      (make-sum
        (make-product
          (multiplier e)
          (deriv (multiplicand e) v))
        (make-product
          (multiplicand e)
          (deriv (multiplier e) v))))
    (else (error "unknown expression type: DERIV" e))))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a b)
  (cond
    ((number=? a 0) b)
    ((number=? b 0) a)
    ((and (number? a) (number? b))
      (+ a b))
    (else (list '+ a b))))

(define (sum? e)
  (and (cons? e) (eq? '+ (car e))))

(define addend cadr)

(define augend caddr)

(define (make-product a b)
  (cond
    ((or (number=? a 0) (number=? b 0))
      0)
    ((number=? a 1) b)
    ((number=? b 1) a)
    ((and (number? a) (number? b))
      (* a b))
    (else (list '* a b))))

(define (product? e)
  (and (cons? e) (eq? '* (car e))))

(define multiplier cadr)

(define multiplicand caddr)


(deriv '(+ 3 x) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
