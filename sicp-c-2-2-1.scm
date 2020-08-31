(define a (list 1 2 3 4))

a
(car a)
(cdr a)
(cadr a)
(cons 5 a)
(cddddr a)
(list)
(car (cddddr a))

(define (nth items n)
  (if (= n 0)
      (car items)
      (nth (cdr items) (- n 1))))

"---"
(define squares (list 0 1 4 9 16 25))
(nth squares 3)

(define (len items)
  (if (empty? items)
      0
      (+ 1 (len (cdr items)))))
         
(len a)
(len squares)

(define (len2 items)
  (letrec ((iter (lambda (items res)
  (if (empty? items)
      res
      (iter (cdr items) (+ 1 res))))))
    (iter items 0)))

(len2 squares)

"---"

(define (app a b)
  (if (empty? a)
      b
      (cons (car a) (app (cdr a) b))))

(app a squares)
(app squares a)