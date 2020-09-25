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


(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(cons? (car '(a short list)))
(memq 'red '(('red shoes) (blue socks)))
(memq 'red '( 'red shoes blue socks))

;(list 'a 'b 'c)
;(list (list 'george))
;(list (list 'y1 'y2))
;(list 'y1 'y2)
;#f
;#f
;(list 'reb 'shoes 'blue 'socks)

