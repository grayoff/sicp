(define (f x y . z)
  (append (list x y "-") z))

(f 1 2 3 4 5)
