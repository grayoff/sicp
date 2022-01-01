(define (div? a b)
  (= (% a b) 0))

(define (next a)
  (if (= a 2) 3 (+ a 2)))

(define (ldiv n)
  (letrec ((iter (lambda (i)
    (if (div? n i)
        i
        (iter (next i))))))
  (iter 2)))

(define (prime-fact n)
  (if (< n 2)
      '()
      (let ((a (ldiv n)))
        (cons a (prime-fact (/ n a))))))


(prime-fact 5040)    