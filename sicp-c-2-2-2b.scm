(define (scale-tree tree n)
  (cond ((null? tree) tree)
        ((not (list? tree))
          (* tree n))
        (else
          (cons
            (scale-tree (car tree) n)
            (scale-tree (cdr tree) n)))))




(define a (list 1 2 (list 3 4 (list 5 6)) 7))

(scale-tree a 10)
         
