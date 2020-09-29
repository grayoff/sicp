(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (first v))

(define (ycor-vect v)
  (second v))

(define (add-vect a b)
  (make-vect (+ (xcor-vect a)
                (xcor-vect b))
             (+ (ycor-vect a)
                (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a)
                (xcor-vect b))
             (- (ycor-vect a)
                (ycor-vect b))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (first frame))

(define (edge1-frame frame)
  (second frame))

(define (edge2-frame frame)
  (third frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect
        (scale-vect
          (xcor-vect v)
          (edge1-frame frame))
        (scale-vect
          (ycor-vect v)
          (edge2-frame frame))))))

(define def-frame
  (make-frame
    (make-vect 20 980)
    (make-vect 680 0)
    (make-vect 0 -680)))

(define (draw-line a b scene)
  (add-line
    scene
    (xcor-vect a)
    (ycor-vect a)
    (xcor-vect b)
    (ycor-vect b)
    "black"))

(define (make-segment start end)
  (list start end))

(define (start-segment s)
  (first s))

(define (end-segment s)
  (second s))
    
(define (segment->painter segment-list)
  (lambda (frame)
  (lambda (scene)
  (letrec ((iter (lambda (segs)
    (if (null? segs)
        scene
        (let ((seg (car segs))
              (trans (frame-coord-map frame)))
          (draw-line
            (trans (start-segment seg))
            (trans (end-segment seg))
            (iter (cdr segs))))))))
  (iter segment-list))))) 


(define a (make-vect 0 0))
(define b (make-vect 1 0))
(define c (make-vect 1 1))
(define d (make-vect 0 1))


(define p
  (segment->painter
    (list (make-segment a b)
          (make-segment b c))))

(define outline
  (segment->painter
    (list (make-segment a b)
          (make-segment b c)
          (make-segment c d)
          (make-segment d a))))

(define cross
  (segment->painter
    (list (make-segment a c)
          (make-segment b d))))

(define diamond
  (let ((ma (scale-vect 0.5
            (add-vect a b)))
        (mb (scale-vect 0.5
            (add-vect b c)))
        (mc (scale-vect 0.5
            (add-vect c d)))
        (md (scale-vect 0.5
            (add-vect d a))))
   (segment->painter
    (list (make-segment ma mb)
          (make-segment mb mc)
          (make-segment mc md)
          (make-segment md ma)))))


(define wave (segment->painter (list (make-segment (make-vect .25 0) (make-vect .35 .5)) (make-segment (make-vect .35 .5) (make-vect .3 .6)) (make-segment (make-vect .3 .6) (make-vect .15 .4)) (make-segment (make-vect .15 .4) (make-vect 0 .65)) (make-segment (make-vect 0 .65) (make-vect 0 .85)) (make-segment (make-vect 0 .85) (make-vect .15 .6)) (make-segment (make-vect .15 .6) (make-vect .3 .65)) (make-segment (make-vect .3 .65) (make-vect .4 .65)) (make-segment (make-vect .4 .65) (make-vect .35 .85)) (make-segment (make-vect .35 .85) (make-vect .4 1)) (make-segment (make-vect .4 1) (make-vect .6 1)) (make-segment (make-vect .6 1) (make-vect .65 .85)) (make-segment (make-vect .65 .85) (make-vect .6 .65)) (make-segment (make-vect .6 .65) (make-vect .75 .65)) (make-segment (make-vect .75 .65) (make-vect 1 .35)) (make-segment (make-vect 1 .35) (make-vect 1 .15)) (make-segment (make-vect 1 .15) (make-vect .6 .45)) (make-segment (make-vect .6 .45) (make-vect .75 0)) (make-segment (make-vect .75 0) (make-vect .6 0)) (make-segment (make-vect .6 0) (make-vect .5 .3)) (make-segment (make-vect .5 .3) (make-vect .4 0)) (make-segment (make-vect .4 0) (make-vect .25 0)) )))
  
        
(show-image
  ((wave def-frame)
  ((diamond def-frame)
  ((cross def-frame)
  ((outline def-frame)
   (empty-scene)))))
    )
