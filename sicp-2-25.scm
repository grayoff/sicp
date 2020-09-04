(define a '(1 3 '(5 7) 9))
(define b '( '(7)))
(define c '(1 '(2 '(3 '(4 '(5 '(6 7)))))))

a
b
c

(cadr (caddr a))
(car (cdr (car (cdr (cdr a)))))

(caar b)
(car (car b))

(cadr (cadr (cadr (cadr (cadr (cadr c))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))