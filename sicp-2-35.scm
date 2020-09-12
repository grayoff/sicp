(define (acc op init seq)
  (if (null? seq)
      init
      (op (car seq) (acc op init (cdr seq)))))

(define (count-leaves2 tree)
  (acc
    (lambda (x y) 
      (+ y (if (list? x)
               (count-leaves2 x)
               1)))
    0
    tree))

(define (count-leaves tree)
  (acc
    +
    0
    (map
      (lambda (x)
        (if (list? x)
            (count-leaves x)
            1))       
     tree)))


(define a '(1 '(2 '(3 4) 5) '(6 7)))

(count-leaves a)
(count-leaves2 a)
 
