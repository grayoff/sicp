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

(define (rev1 seq)
  (foldr2 (lambda (x y) (append y (list x))) '() seq))

(define (rev2 seq)
  (foldl3 (lambda (x y) (cons y x)) '() seq))

(define (rev3 seq)
  (foldr2 (lambda (x y)
            (foldr cons (list x) y))
          '()
          seq))


(define a '(1 2 3 4))

(rev1 a)
(rev2 a)
(rev3 a)