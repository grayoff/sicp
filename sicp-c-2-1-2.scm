(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (% a b))))
      
(define (make-rat n d)
  (list n d))


(define (numer x)
  (let ((n (first x))
        (d (second x)))
    (let ((g (abs (gcd n d))))
      (/ ((if (> d 0) + - ) n) g))))
          

(define (denom x)
  (let ((n (first x))
        (d (second x)))
    (let ((g (abs (gcd n d))))
      (/ ((if (> d 0) + -) d) g))))

(define (print-rat x)
  (string-append
    (number->string (numer x))
    "/"
    (number->string (denom x))))
         
(define (add-rat x y)
  (make-rat
    (+ (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
    (- (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat
    (* (numer x) (denom y))
    (* (denom x) (numer y))))

(define (eq-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define a (make-rat 1 2))
(define b (make-rat 1 3))
(define z (make-rat 0 1))
(define i (make-rat 1 1))

(print-rat a)
(print-rat b)
(print-rat z)
"---"
(print-rat (add-rat a b))
(print-rat (add-rat b b))
(print-rat (add-rat b z))
"---"
(print-rat (sub-rat a b))
(print-rat (sub-rat b z))
(print-rat (sub-rat a a))
"---"
(print-rat (mul-rat a b))
"---"
(print-rat (div-rat a b))
"---"
(eq-rat? a b)
(eq-rat? a a)
"---"
(print-rat (make-rat -3 -6))
(print-rat (make-rat -3 6))
(print-rat (make-rat 3 -6))

