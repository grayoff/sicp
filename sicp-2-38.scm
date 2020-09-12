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

(define a '(1.0 2.0 3.0))

(foldr cons '() a)
(foldr2 cons '() a)
(foldl cons '() a)
(foldl2 (flip cons) '() a)

"---"

(foldr2 / 1.0 a)
; (/ 1 (/ 2 (/ 3 1)))
; 1.5
(foldl2 / 1.0 a)
; (/ (/ (/ 1 1) 2) 3))
; 0.166..
(foldr2 list '() a)
; (list 1 (list 2 (list 3 (list )))
(foldl2 list '() a)
; (list (list (list (list ) 1) 2) 3)

"---"

(foldr2 + 0 a)
(foldl2 + 0 a)
(foldr2 * 1 a)
(foldl2 * 1 a)
; commutative and associative
(foldr2 expt 2 a)
(foldl2 expt 2 a)

"---"

(foldl3 / 1.0 a)
(foldl3 list '() a)
(foldl3 + 0 a)
(foldl3 * 1 a)
(foldl3 expt 2 a)