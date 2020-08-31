#lang racket
(define (f x y . z)
  (append (list x y "-") z))

(define (g . w)
  w)

(define f1 (lambda (x y . z) (append (list x y "-") z)))

(define g1 (lambda w w))

(f 1 2 3 4 5)
(g 1 2 3 4 5)
(f1 1 2 3 4 5)
(g1 1 2 3 4 5)

(define (same-parity x . z)
  (let ((eveness (even? x)))
    (cons x (filter (lambda (y) (eq? (even? y) eveness)) z))))

(define (same-parity2 x . z)
  (let ((eveness (even? x)))
    (define (iter eveness z)
      (if (empty? z)
          z
          (let ((y (car z)))
            (if (eq? (even? y) eveness)
                (cons y (iter eveness (cdr z)))
                (iter eveness (cdr z))))))
     (cons x (iter eveness z))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(same-parity2 1 2 3 4 5 6 7)
(same-parity2 2 3 4 5 6 7)

