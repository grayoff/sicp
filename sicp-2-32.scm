(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
        (append
          rest
          (map (lambda (x) (cons (car s) x)) rest)))))

(define (subsets2 s)
  (if (null? s)
      (list s)
      (let ((rest (subsets2 (cdr s))))
        (append
          (map (lambda (x) (cons (car s) x)) rest)
          rest))))

(define a '(1 2 3))

(subsets a)
(subsets2 a)

(subsets '(1))

