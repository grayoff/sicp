(define (scale-list items factor)
  (if (empty? items)
      items
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map2 proc items)
  (if (empty? items)
      items
      (cons (proc (car items)) (map proc (cdr items)))))

(define (scale-list2 items factor)
  (map (lambda (x) (* x factor)) items))

(define a (list 1 2 3 4 5))

(scale-list a 10)
(map2 abs (list -1 2 -4 -5))
(scale-list2 a 10)

(map + a (scale-list a 10))

