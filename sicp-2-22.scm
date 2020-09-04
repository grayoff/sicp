(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list2 items)
  (map square items))

(define (square-list3 items)
  (letrec ((iter (lambda (a r)
    (if (null? a)
        r
        (iter (cdr a)
              (list (square (car a)) r))))))
  (iter items '())))

(define (square-list3a items)
  (letrec ((iter (lambda (a r)
    (if (null? a)
        r
        (iter (cdr a)
              (list r (square (car a)) ))))))
  (iter items '())))

(define (square-list3b items)
  (letrec ((iter (lambda (a r)
    (if (null? a)
        r
        (iter (cdr a)
              (append r (list (square (car a)) )))))))
  (iter items '())))

(define (id x) x)

(define (square-list3c items)
  (letrec ((iter (lambda (a pick)
    (if (null? a)
        (pick a)
        (iter (cdr a)
              (lambda (x) (pick (cons (square (car a)) x))))))))
  (iter items id)))


(define a (list 1 2 3 4))
(square-list a)
(square-list2 a)
(square-list3 a)
(square-list3a a)
(square-list3b a)
(square-list3c a)
