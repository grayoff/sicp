(define (make-interval a b)
  (list a b))

(define (lower x)
  (first x))

(define (upper x)
  (second x))

(define (width x)
  (/ (- (upper x) (lower x)) 2))

(define (print-interval x)
  (string-append
    "["
    (number->string (lower x))
    ","
    (number->string (upper x))
    "]"))

(define (add x y)
  (make-interval
    (+ (lower x) (lower y))
    (+ (upper x) (upper y))))

(define (sub x y)
  (add
    x
    (make-interval
      (- (upper y))
      (- (lower y)))))

;++ ++  13 24
;++ -+  23 24
;++ --  23 14

;-+ ++  14 24
;-+ -+  min 14 23 max 13 24
;-+ -- 23 13

;-- ++  14 23
;-- -+  14 13
;-- --  24 13
(define (mul x y)
  (let ((s1 (lower x))
        (s2 (upper x))
        (s3 (lower y))
        (s4 (upper y)))
    (if (>= s1 0)
        (if (>= s3 0)
            (make-interval
              (* s1 s3)
              (* s2 s4))
            (if (>= s4 0)
                (make-interval
                  (* s2 s3)
                  (* s2 s4))
                (make-interval
                  (* s2 s3)
                  (* s1 s4))))

         (if (>= s2 0)
             (if (>= s3 0)
                 (make-interval
                   (* s1 s4)
                   (* s2 s4))
                 (if (>= s4 0)
                     (make-interval
                       (min (* s1 s4)
                            (* s2 s3))
                       (max (* s1 s3)
                            (* s2 s4)))
                     (make-interval
                       (* s2 s3)
                       (* s1 s3))))
              (if (>= s3 0)
                  (make-interval
                    (* s1 s4)
                    (* s2 s3))
                  (if (>= s4 0)
                      (make-interval
                        (* s1 s4)
                        (* s1 s3))
                      (make-interval
                        (* s2 s4)
                        (* s1 s3)))
               )))))
         

(define (mul2 x y)
  (let ((p1 (* (lower x) (lower y)))
        (p2 (* (lower x) (upper y)))
        (p3 (* (upper x) (lower y)))
        (p4 (* (upper x) (upper y))))
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div x y)
  (let ((ya (lower y))
        (yb (upper y)))
    (if (and (< ya 0) (> yb 0))
        (error "Division by zero span interval")
        (mul
          x
          (make-interval
            (/ 1.0 yb)
            (/ 1.0 ya))))))


(define a (make-interval 1 2))
(define b (make-interval -1 1))
(define c (make-interval -2 -1))

(print-interval a)
(print-interval b)
(print-interval c)

"---"

(print-interval (mul a a))
(print-interval (mul2 a a))
(print-interval (mul a b))
(print-interval (mul2 a b))
(print-interval (mul a c))
(print-interval (mul2 a c))

(print-interval (mul b a))
(print-interval (mul2 b a))
(print-interval (mul b b))
(print-interval (mul2 b b))
(print-interval (mul b c))
(print-interval (mul2 b c))

(print-interval (mul c a))
(print-interval (mul2 c a))
(print-interval (mul c b))
(print-interval (mul2 c b))
(print-interval (mul c c))
(print-interval (mul2 c c))