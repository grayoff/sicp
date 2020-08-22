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

(define (rect-make p w h r)
  (list p w h))

(define (rect-orig x)
  (first x))

(define (rect-w x)
  (second x))

(define (rect-h x)
  (third x))

(define (rect-r x)
  (fourth x))

(define (rect-p r)
  (* 2 (+ (rect-w r) (rect-h r))))

(define (rect-s r)
  (* (rect-w r) (rect-h r)))



(define a (pt-make 1 4))
(define r (rect-make a 5 2 1))

(rect-p r)
(rect-s r)


(define (rect-make2 p1 p2)
  (list p1 p2))

(define (rect-orig x)
  (first x))

(define (rect-w x)
  (abs (- (pt-x (second x))
          (pt-x (first x)))))

(define (rect-h x)
    (abs (- (pt-y (second x))
          (pt-y (first x)))))

(define (rect-r x)
  0)

(define a (pt-make 1 4))
(define b (pt-make 6 6))
(define r (rect-make2 a b))

(rect-p r)
(rect-s r)
