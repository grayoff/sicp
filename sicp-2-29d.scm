(define (cons2 a b)
  (lambda (x)
    (cond ((= x 0) a)
          ((= x 1) b)
          (else (error "out of range")))))

(define (car2 x) (x 0))

(define (cdr2 x) (x 1))
  

(define (make-mob left right)
  (cons2 left right)) ;+

(define (make-branch length struct)
  (cons2 length struct)) ;+

; a

(define left-branch car2)

(define right-branch cdr2) ;+

(define branch-length car2)

(define branch-struct cdr2) ;+

; b

(define (total-weight x)
 (if (number? x)
  x
  (+ (branch-weight (left-branch x))
     (branch-weight (right-branch x)))))

(define (branch-weight x)
  (total-weight (branch-struct x)))
       

; c

(define (balanced? x)
  (if (number? x)
      #t
      (let ((a (left-branch x))
            (b (right-branch x)))
        (and
          (= (torque a) (torque b))
          (balanced? (branch-struct a))
          (balanced? (branch-struct b))
))))

(define (torque x)
  (* (branch-length x)
     (total-weight (branch-struct x))))


(define a (make-mob
            (make-branch 3 5)
            (make-branch 5 3)))

(total-weight a)
(balanced? a)

(define b (make-mob
            (make-branch 5 a)
            (make-branch 5 1)))

(total-weight b)
(balanced? b)

(define c (make-mob
            (make-branch 5 a)
            (make-branch 4
              (make-mob
                (make-branch 8 2)
                (make-branch 2 a)))))

(total-weight c)
(balanced? c)
