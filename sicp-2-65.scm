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

(define (union-set a b)
  (list->tree
    (union-list
      (tree->list-2 a)
      (tree->list-2 b))))

(define (intersection-set a b)
  (list->tree
    (intersect-list
      (tree->list-2 a)
      (tree->list-2 b))))

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

;-------

(define (union-list a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else
          (let ((x (car a))
                (y (car b)))
          (cond
            ((= x y) (cons x (union-list (cdr a) (cdr b))))
            ((< x y) (cons x (union-list (cdr a) b)))
            (else (cons y (union-list a (cdr b)))))))))

(define (intersect-list a b)
  (cond ((null? a) '())
        ((null? b) '())
        (else
          (let ((x (car a))
                (y (car b)))
          (cond
            ((= x y) (cons x (intersect-list (cdr a) (cdr b))))
            ((< x y) (intersect-list (cdr a) b))
            (else (intersect-list a (cdr b))))))))   
       
       
    
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

(define d (list->tree '(1 2 3 5)))

(define e (list->tree '(2 4 5 6 7)))


;(list->tree '(1 3 5 7 9 11))
;(union-list '(1 2 3 5) '(2 4 5 6 7))
;(intersect-list '(1 2 3 5) '(2 4 5 6 7))
(tree->list-2 (union-set d e))
(tree->list-2 (intersection-set d e))
(tree->list-2 (union-set (list->tree '(1 2 3 5 7 9 46)) (list->tree '(5 6 10 11 20 23 46))))