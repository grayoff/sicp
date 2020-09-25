(define (eq? a b)
  (if (and (symbol? a) (symbol? b))
      (symbol=? a b)
      #f))

(define (memq item x)
  (if (null? x)
      false
      (if (eq? item (car x))
          x
          (memq item (cdr x)))))


(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) apple pear))