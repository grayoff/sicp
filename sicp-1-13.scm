(define (pascal-tri r c)
  (if (or (= c 1) (= c r))
    1
    (+ (pascal-tri (- r 1) (- c 1))
       (pascal-tri (- r 1) c))))
(pascal-tri 5 3)
(pascal-tri 5 4)