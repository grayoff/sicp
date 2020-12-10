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

(define (element-of-set? x s)
  (if (null? s)
      #f
      (if (equal? x (car s))
          #t
          (element-of-set? x (cdr s)))))

(define (adjoin-set x s)
  (cons x s))

(define (intersection-set a b)
  (if (or (null? a) (null? b))
      '()
      (let ((x (car a)))
        (if (element-of-set? x b)
            (cons x (intersection-set (cdr a) b))
            (intersection-set (cdr a) b)))))

(define (union-set a b)
  (append a b))


(element-of-set? 3 '(1 2 3 4))
(element-of-set? 5 '(1 2 3 4))
(intersection-set '(1 2 3 4) '(5 4 7 6 3))
(union-set '(1 2 3 4) '(5 4 7 6 3))
