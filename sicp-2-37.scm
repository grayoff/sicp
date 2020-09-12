(define (acc op init seq)
  (if (null? seq)
      init
      (op (car seq) (acc op init (cdr seq)))))

(define (acc-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons
        (acc op init (map car seqs))
        (acc-n op init (map cdr seqs)))))

(define (map2 f a b)
  (if (or (null? a) (null? b))
      '()
      (cons (f (car a) (car b))
            (map2 f (cdr a) (cdr b)))))

(define (dot v w)
  (acc + 0 (map2 * v w)))

(define (m*v m v)
  (map (lambda (x) (dot x v)) m))

(define (v*m v m)
  (if (null? (car m))
      '()
      (cons (dot v (map car m))
            (v*m v (map cdr m)))))

(define (trans m)
  (acc-n cons '() m))

(define (m*m m n)
  (let ((cols (trans n)))
    (map
      (lambda (x) (m*v cols x))
      m)))

(define (v*m2 v m)
  (let ((cols (trans m)))
    (map
      (lambda (x) (dot v x))
      cols)))


(define a '(1 2 3 4))
(define b '( '(1 2 3 4)
             '(4 5 6 6)
             '(6 7 8 9)))
(define c '(1 2 3))
(define d '(3 4 5))
(define e '( '(1 2 3)
             '(3 2 1)
             '(2 1 3)))
(define e1 '( '(1 2 3)
             '(3 2 1)
             '(2 1 3)
             '(2 2 2)))

(dot a a)
(dot c d)
(m*v e d)
(m*v e1 d)
(v*m '(3 4 5 5) e1)
(trans e1)
(m*m '( '(3 4 5 5)) e1)
(m*m b e1)
(v*m2 '(3 4 5 5) e1)