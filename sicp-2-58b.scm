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

(define (take-before s e)
  (if (null? e)
      '()
      (let ((a (car e)))
      (if (eq? s a)
          '()
          (cons a (take-before s (cdr e)))))))

(define (unpack e)
  (if (and (cons? e) (null? (cdr e)))
      (car e)
      e))

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
    ((expt? e)
      (let ((b (base e))
            (p (power e)))
      (make-product
        (make-product
          p
          (make-expt b (- p 1)))
        (deriv b v))))
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
    (else (list a '+ b))))

(define (sum? e)
  (and (cons? e) (cons? (memq '+ e))))

(define (addend e)
  (unpack (take-before '+ e)))

(define (augend e)
  (unpack (cdr (memq '+ e))))         

(define (make-product a b)
  (cond
    ((or (number=? a 0) (number=? b 0))
      0)
    ((number=? a 1) b)
    ((number=? b 1) a)
    ((and (number? a) (number? b))
      (* a b))
    (else (list a '* b))))

(define (product? e)
  (and (cons? e) (cons? (memq '* e))))

(define (multiplier e)
  (unpack (take-before '* e)))

(define (multiplicand e)
  (unpack (cdr (memq '* e))))

(define (make-expt a b)
  (cond
    ((number=? b 0) 1)
    ((number=? b 1) a)
    ;((number=? a 0) 0)
    ;((number=? a 1) 1)
    ((and (number? a) (number? b))
      (expt a b))
    (else (list a 'expt b))))
    
(define (expt? e)
  (and (cons? e) (cons? (memq 'expt e))))

(define (base e)
  (unpack (take-before 'expt e)))

(define (power e)
  (unpack (cdr (memq 'expt e))))

       


'(1 + 2 * 3 + 4) ; 11
'(1 * 2 + 3 * 4) ; 14

(define a '(1 + 2 + x + 3 + y))

(deriv a 'x)
(deriv '(x * 2 + x * 4 + 5 * x expt 2) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)



