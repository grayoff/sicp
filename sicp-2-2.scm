(define (seg-make p1 p2)
  (list p1 p2))

(define (seg-beg s)
  (first s))

(define (seg-end s)
  (second s))

(define (seg-mid s)
  (pt-scale (pt-add (seg-beg s) (seg-end s)) 0.5))

(define (pt-make x y)
  (list x y))

(define (pt-x p)
  (first p))

(define (pt-y p)
  (second p))

(define (pt-add p1 p2)
  (pt-make (+ (pt-x p1) (pt-x p2))
           (+ (pt-y p1) (pt-y p2))))

(define (pt-scale p size)
  (pt-make (* (pt-x p) size)
           (* (pt-y p) size)))

(define (pt-print p)
  (string-append
    "("
    (number->string (pt-x p))
    ","
    (number->string (pt-y p))
    ")"))


(define a (pt-make 1 4))
(define b (pt-make 4 1))

(define s (seg-make a b))

(pt-print (seg-mid s))



 

