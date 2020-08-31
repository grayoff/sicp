(define (cc amount coins)
  (letrec ((no-more? empty?)
           (first-denom car)
           (except-first-denom cdr))
  (cond ((or (< amount 0)
             (no-more? coins)) 0)
        ((= amount 0) 1)
        (else
         (+ (cc (- amount (first-denom coins))
               coins)
            (cc amount (except-first-denom coins)))))))

(define (cc2 amount coins)
  (cond ((or (< amount 0)
             (empty? coins)) 0)
        ((= amount 0) 1)
        (else
         (+ (cc2 (- amount (car coins))
               coins)
            (cc2 amount (cdr coins))))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
(cc 100 (list 25 1 5 10 50))
