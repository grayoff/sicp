(define acc foldr)

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

(define (flip f)
  (lambda (x y) (f y x)))

(define (range a b)
  (if (> a b)
      '()
      (cons a (range (+ a 1) b))))

(define (flatmap proc seq)
  (acc append '() (map proc seq)))

(define (queens board-size)
  (letrec ((queen-cols (lambda (k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (p)
                  (safe? k p))
        (flatmap
          (lambda (r)
            (map (lambda (i)
                   (adjoin i k r))
                 (range 1 board-size)))
            (queen-cols (- k 1)))))))) 
  (queen-cols board-size)))

(define empty-board '())

(define (safe? k p)
  (let ((iq (caar p))
        (kq (cadar p)))
    (not (ormap (lambda (pos)
      (let ((ip (car pos))
            (kp (cadr pos))) 
       (or (= iq ip)
           (= kq kp)
           (= (- iq kq) (- ip kp))
           (= (+ iq kq) (+ ip kp))           
       )
      ))
      (cdr p)))))

(define (adjoin i k r)
  (cons (list i k) r))


(queens 5)
