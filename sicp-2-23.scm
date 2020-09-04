(define (for-each proc items)
  (if (null? items)
      #t
      (begin (proc (car items))
             (for-each proc (cdr items)))))


(define a (list 1 2 3 4))

(for-each println a)
