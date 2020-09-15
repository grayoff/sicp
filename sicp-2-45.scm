#lang sicp
(#%require sicp-pict)

(define wave2 (beside einstein (flip-vert einstein)))
(define wave4 (below wave2 wave2))

(define (fliped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4-2 (fliped-pairs einstein))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

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

(define fliped-pairs2 (square-of-four identity flip-vert identity flip-vert))

(define wave4-3 (fliped-pairs2 einstein))

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (split comb1 comb2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
        (comb1 painter (comb2 smaller smaller)))))
  iter)

(define right-split2 (split beside below))
(define up-split2 (split below beside))

(paint einstein)
(paint (beside einstein mark-of-zorro))
(paint (below einstein mark-of-zorro))
(paint (flip-vert einstein))
(paint (flip-horiz einstein))
(paint wave2)
(paint wave4)
(paint wave4-2)
(paint wave4-3)
(paint (right-split einstein 4))
(paint (up-split einstein 4))
(paint (corner-split einstein 4))
(paint (square-limit einstein 4))
(paint (square-limit mark-of-zorro 4))
(paint (square-limit2 einstein 4))
(paint (right-split2 einstein 4))
(paint (up-split2 einstein 4))

