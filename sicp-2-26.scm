(define x (list 1 2 3))
(define y (list 4 5 6))

; (list 1 2 3 4 5 6)
; (list (list 1 2 3) 4 5 6)
; (list (list 1 2 3) (list 4 5 5))

(append x y)
(cons x y)
(list x y)