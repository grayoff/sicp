(define (acc op init seq)
  (if (null? seq)
      init
      (op (car seq) (acc op init (cdr seq)))))

(define (acc-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons
        (acc op init (map car seqs))
        (acc-n op init (map cdr seqs)))))

(define (map2 f a b)
  (if (or (null? a) (null? b))
      '()
      (cons (f (car a) (car b))
            (map2 f (cdr a) (cdr b)))))

(define foldr2 acc)

(define (foldl2 op init seq)
  (letrec ((iter (lambda (result items)
    (if (null? items)
        result
        (iter (op result (car items))
              (cdr items))))))
    (iter init seq)))

(define (foldl3 op init seq)
  (if (null? seq)
      init
      (foldl3 op
             (op init (car seq))
             (cdr seq))))

(define (flip f)
  (lambda (x y) (f y x)))

(define (range a b)
  (if (> a b)
      '()
      (cons a (range (+ a 1) b))))

(define (flatmap proc seq)
  (acc append '() (map proc seq)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (% a b))))

(define (prime? n)
  (if (< n 2) #f
  (andmap (lambda (x) (= (gcd n x) 1))
          (range 2 (floor (sqrt n))))))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (range 0 (- i 1))))
    (range 0 n)))

(define (tri-sum n s)
  (filter (lambda (x) (= s (acc + 0 x)))
  (flatmap
    (lambda (x)
      (map (lambda (y)
             (list (cadr y) (car y) x))
           (unique-pairs (- x 1))))
    (range 0 n))))


(tri-sum 6 7)
(tri-sum 7 11)