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

(define (memq item x)
  (if (null? x)
      false
      (if (eq? item (car x))
          x
          (memq item (cdr x)))))

;-------

(define (make-tree entry left right)
  (list entry left right))

(define (entry t)
  (car t))

(define (left-branch t)
  (cadr t))

(define (right-branch t)
  (caddr t))

;-------

(define (element-of-set? x s)
  (if (null? s)
      false
      (let ((e (entry s)))
        (cond  
          ((= x e) true)
          ((< x e) (element-of-set? x (left-branch s)))
          (else (element-of-set? x (right-branch s)))))))

(define (adjoin-set x s)
  (if (null? s)
      (make-tree x '() '())
      (let ((e (entry s)))
        (cond  
          ((= x e) s)
          ((< x e)
            (make-tree
              e
              (adjoin-set x (left-branch s))
              (right-branch s)))
          (else
            (make-tree
              e
              (left-branch s)
              (adjoin-set x (right-branch s)))))))) 

;-------

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
        (tree->list-1 (left-branch tree))
        (cons (entry tree)
              (tree->list-1 (right-branch tree))))))
              
(define (tree->list-2 tree)
  (letrec ((copy-to-list (lambda (t res)
  (if (null? t)
      res
      (copy-to-list
        (left-branch t)
        (cons (entry t)
              (copy-to-list (right-branch t) res)))))))  
  
  (copy-to-list tree '())))

;-------

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((lnum (/ (- n 1) 2)))
      (let ((lres (partial-tree elts lnum)))
      (let ((ltree (car lres))
            (lrest-elts (cdr lres)))
      (let ((rnum (- n lnum 1)))
      (let ((rres (partial-tree (cdr lrest-elts) rnum)))
      (let ((rtree (car rres))
            (rrest-elts (cdr rres)))
        (cons
          (make-tree
            (car lrest-elts)
            ltree
            rtree)
          rrest-elts)))))))))

  
       
       
    
(define a '(7 (3 (1 () ())
                 (5 () ()))
              (9 ()
                 (11 () ()))))

(define b '(3 (1 () ())
              (7 (5 () ())
                 (9 () (11 () ())))))
                  
(define c '(5 (3 (1 () ())
                 ())
              (9 (7 () ())
                 (11 () ()))))


(list->tree '(1 3 5 7 9 11))
c
