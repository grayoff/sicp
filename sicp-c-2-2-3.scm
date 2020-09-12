(define (square x) (* x x))

(define (sum-odd-sq tree)
  (cond ((null? tree) 0)
        ((not (list? tree))
          (if (odd? tree)
              (square tree)
              0))
        (else
          (+ (sum-odd-sq (car tree))
             (sum-odd-sq (cdr tree))))))

(define (fib n)
  (let ((sq5 (sqrt 5.0)))
    (floor
    (/ (- (expt (/ (+ 1.0 sq5) 2.0) n)
          (expt (/ (- 1.0 sq5) 2.0) n))
       sq5))))

(define (even-fib2 n)
  (if (< n 0)
      '()
      (let ((f (fib n)))
       (if (even? f)
         (append (even-fib2 (- n 1)) 
                 (list f))
         (even-fib2 (- n 1))))))

(define (even-fib n)
  (letrec ((next (lambda (k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))))
    (next 0)))

(define (filter pred seq)
  (if (null? seq)
      seq
      (let ((a (car seq)))
        (if (pred a)
            (cons a (filter pred (cdr seq)))
            (filter pred (cdr seq))))))

(define (acc op init seq)
  (if (null? seq)
      init
      (op (car seq) (acc op init (cdr seq)))))

(define (range a b)
  (if (> a b)
      '()
      (cons a (range (+ a 1) b))))

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (list? tree)) (list tree))
        (else
          (append (fringe (car tree))
                  (fringe (cdr tree))))))

(define (sum-odd-sq2 tree)
  (acc + 0 (map square (filter odd? (fringe tree)))))

(define (even-fib3 n)
  (filter even? (map fib (range 0 n))))

(define (fib-sq n)
  (map square (map fib (range 0 n))))

(define (pr-sq-odd seq)
  (acc * 1 (map square (filter odd? seq))))

(define (mk-rec name age pos salary)
  (list name age pos salary))

(define (name x) (first x))

(define (age x) (second x))

(define (pos x) (third x))

(define (salary x) (fourth x))

(define (programmer? x)
  (string=? (pos x) "programmer"))

(define (max-prog-sal recs)
  (acc max 0 (map salary (filter programmer? recs))))

(define a '(1 '(2 '(3 4) 5) '(6 7)))
(define b '(0 1 2 3 4 5 6 7 8 9 10))
(define c '(1 2 3 4 5))

 
(define records (list
  (mk-rec "nio" 32 "programmer" 1200)
  (mk-rec "zso" 45 "account" 850)
  (mk-rec "zme" 50 "cto" 2500)
  (mk-rec "peo" 37 "programmer" 1800)
  (mk-rec "zzt" 29 "programmer" 1000)))


(sum-odd-sq a)
(map fib b)
(even-fib 10)
(even-fib2 10)
(filter odd? b)
(acc cons '() b)
(foldr cons '() b)
(foldl cons '() b)
"---"
(filter odd? c)
(acc + 0 c)
(acc * 1 c)
(acc cons '() c)
"---"
(range 1 5)
(fringe a)
"---"
(sum-odd-sq2 a)
(even-fib3 10)
(fib-sq 10)
(pr-sq-odd c)
"---"
(acc max 0 '(3 9 8 1 2)) 
;(apply max '(3 9 8 1 2))
"---"
(max-prog-sal records)

