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
  (cond ((null? s) false)  
        ((equal? x (car s)) true)
        ((< x (car s)) false)
        (else (element-of-set? x (cdr s)))))

(define (adjoin-set x s)
  (if (or (null? s) (< x (car s)))
      (cons x s)
      (cons (car s) (adjoin-set x (cdr s)))))

(define (intersection-set a b)
  (if (or (null? a) (null? b))
      '()
      (let ((x1 (car a))
            (x2 (car b)))
        (cond 
          ((= x1 x2) 
            (cons x1 (intersection-set (cdr a) (cdr b))))
          ((< x1 x2)
            (intersection-set (cdr a) b))
          (else (intersection-set a (cdr b)))))))

(define (union-set a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else
          (let ((x1 (car a))
                (x2 (car b)))
          (cond
            ((= x1 x2)
              (cons x1 (union-set (cdr a) (cdr b))))
            ((< x1 x2)
              (cons x1 (union-set (cdr a) b)))
            (else (cons x2 (union-set a (cdr b)))))))))
              
         
    


(element-of-set? 3 '(1 2 3 4))
(element-of-set? 5 '(1 2 3 4))
(intersection-set '(1 2 3 4) '(3 4 5 6 7))
(adjoin-set 3 '(1 2 4 5))
(union-set '(1 2 3 4) '(3 4 5 6 7))
(union-set '(4 5 6) '(3 4 6 7))
