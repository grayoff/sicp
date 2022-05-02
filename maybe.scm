(define none '())

(define just list)

(define (has-value? m) (not (null? m)))

(define value car)

(define (fmap m f)
  (if (has-value? m)
      (just (f (value m)))
      m))

(define (bind m f)
  (if (has-value? m)
      (f (value m))
      m))

(define (value-or m v)
  (if (has-value? m) (value m) v))


(define (inc x) (+ x 1))


(define a (just 0))

(value-or (fmap (fmap a inc) inc) "none")