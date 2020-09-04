(define (scale-tree tree n)
  (cond ((null? tree) tree)
        ((not (list? tree))
          (* tree n))
        (else
          (cons
            (scale-tree (car tree) n)
            (scale-tree (cdr tree) n)))))

(define (scale-tree2 tree n)
  (map
    (lambda (subtree)
      (if (list? subtree)
          (scale-tree2 subtree n)
          (* subtree n)))
    tree))

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (list? tree))
          (square tree))
        (else
          (cons
            (square-tree (car tree))
            (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map
    (lambda (subtree)
      (if (list? subtree)
          (square-tree2 subtree)
          (square subtree)))
    tree))


(define a (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(scale-tree a 10)
(scale-tree2 a 10)  

(square-tree a)
(square-tree2 a)       