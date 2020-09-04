(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (list? tree)) (list tree))
        (else
          (append (fringe (car tree))
                  (fringe (cdr tree))))))

(define (fringe2 tree)
  (if (null? tree)
      tree
      (let ((a (car tree)))
        (if (not (list? a)) 
            (cons
              a
              (fringe2 (cdr tree)))
            (append
              (fringe2 a)
              (fringe2 (cdr tree)))))))

(define x (list (list 1 2) (list 3 4)))


(fringe x)
(fringe (list x x))

(fringe2 x)
(fringe2 (list x x))