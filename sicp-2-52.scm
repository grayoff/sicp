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

;-------------

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

;-------------

(define (draw-line a b scene)
  (add-line
    scene
    (xcor-vect a)
    (ycor-vect a)
    (xcor-vect b)
    (ycor-vect b)
    "black"))

;-------------

(define (make-segment start end)
  (list start end))

(define (start-segment s)
  (first s))

(define (end-segment s)
  (second s))

;-------------
    
(define (segment->painter segment-list)
  (lambda (scene)
  (lambda (frame)
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

;-------------

(define (transform-painter painter origin corner1 corner2)
  (lambda (scene)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
    (let ((new-origin (m origin)))
      ((painter scene)
        (make-frame
          new-origin
          (sub-vect (m corner1)
                    new-origin)
          (sub-vect (m corner2)
                    new-origin))))))))

;-------------

(define (flip-vert painter)
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))
   
(define (rotate-90 painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))

(define (rotate-180 painter)
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

(define (shrink-ur painter)
  (transform-painter
    painter
    (make-vect 0.5 0.5)
    (make-vect 1.0 0.5)
    (make-vect 0.5 1.0)))

(define (squash-inwards painter)
  (transform-painter
    painter
    (make-vect 0.0 0.0)
    (make-vect 0.65 0.35)
    (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (lambda (scene)
  (let ((split-point
          (make-vect 0.5 0.0)))
  (let ((paint-left
          (transform-painter painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
        (paint-right
          (transform-painter painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
  (lambda (frame)
    ((paint-left ((paint-right scene) frame)) frame))))))

(define (below painter1 painter2)
  (lambda (scene)
  (let ((split-point
          (make-vect 0.0 0.5)))
  (let ((paint-down
          (transform-painter painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point))
        (paint-up
          (transform-painter painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
  (lambda (frame)
    ((paint-down ((paint-up scene) frame)) frame))))))

;-------------

(define (flipped-pairs painter)
  (let((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter smaller))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter smaller))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (below (beside (bl painter) (br painter))
           (beside (tl painter) (tr painter)))))

(define (split comb1 comb2)
  (letrec ((iter (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
        (comb1 painter (comb2 smaller smaller)))))))
  iter))

(define (identity x) x)

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four identity flip-horiz flip-vert rotate-180)))
    (combine4 (corner-split painter n))))
;-------------


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
          (make-segment md ma)
          (make-segment mb md)))))

;-------------




        
(show-image
  ((
   (square-limit2 diamond 4)
    

   (empty-scene))
   def-frame)

  )
