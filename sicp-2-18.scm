(define (rev a)
  (letrec ((iter (lambda (items res)
    (if (empty? items)
        res
        (iter (cdr items) (cons (car items) res))))))
    (iter a (list))))

(define (rev2 a)
  (foldl cons '() a))
          
(define a (list 1 2 3 4))
(define sq (list 0 1 4 9 16 25))

(rev a)
(rev sq)
(rev2 sq)

(define sa string-append)
(sa "a" (sa "b" (sa "c" "")))
(foldr sa "" '("a" "b" "c"))
(foldr cons '() '("a" "b" "c"))
(sa (sa (sa "" "c") "b") "a")
(foldl sa "" '("a" "b" "c"))
(foldl cons '() '("a" "b" "c"))

(foldl - 0 '(1 2 3 4 5 6 7 8 9 10))
(foldr - 0 '(1 2 3 4 5 6 7 8 9 10))