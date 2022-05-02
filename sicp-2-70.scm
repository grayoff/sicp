(define none '())

(define just list)

(define (has-value? m) (not (null? m)))

(define value car)

(define (fmap m f)
  (if (has-value? m)
      (just (f (value m)))
      m))

(define (bind m f)
  (if (has-value? m)
      (f (value m))
      m))

(define (value-or m v)
  (if (has-value? m) (value m) v))

;-------

(define (eq? a b)
  (cond
    ((and (symbol? a) (symbol? b))
      (symbol=? a b))
    ((and (string? a) (string? b))
      (string=? a b))
    ((and (number? a) (number? b))
      (= a b))
    ((and (char? a) (char? b))
      (char=? a b)) 
    (else #f)))

(define (equal? a b)
  (if (and (list? a) (list? b))
      (cond
        ((and (null? a) (null? b)) #t)
        ((or (null? a) (null? b)) #f)
        (else
          (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b)))))    
      (eq? a b)))

(define (number=? a b)
  (and (number? a) (= a b)))

(define (memq item x)
  (if (null? x)
      false
      (if (eq? item (car x))
          x
          (memq item (cdr x)))))

;-------

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? x)
  (and (cons? x) (eq? 'leaf (car x))))

(define symbol-leaf cadr)

(define weight-leaf caddr)

;-------

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define left-branch car)

(define right-branch cadr)

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;-------

(define (decode bits tree)
  (letrec ((dec (lambda (b t)
    (if (null? b)
        '()
        (let ((branch (choose-branch (car b) t)))
        (if (leaf? branch)
            (cons (symbol-leaf branch) (dec (cdr b) tree))
            (dec (cdr b) branch)))))))
  (dec bits tree)))

(define (choose-branch bit tree)
  (cond ((= 0 bit) (left-branch tree))
        ((= 1 bit) (right-branch tree))
        (else (error "Incorrect bit"))))

(define (decode2 bits tree)
  (letrec ((dec (lambda (b t)
    (cond
      ((leaf? t)
        (cons (symbol-leaf t)
              (dec b tree)))
      ((null? b) '())
      (else
        (if (= 0 (car b))
            (dec (cdr b) (left-branch t))
            (dec (cdr b) (right-branch t))))))))
  (dec bits tree)))
  
;-------

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-set-leaf pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
          (make-leaf (car pair)
                     (cadr pair))
          (make-set-leaf (cdr pairs))))))

;-------

(define (encode message tree)
  (if (null? message)
      '()
      (append
        (encode-symbol (car message) tree)
        (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((lb (left-branch tree))
            (rb (right-branch tree)))
      (cond
        ((element-of-set? symbol (symbols lb)) (cons 0 (encode-symbol symbol lb)))
        ((element-of-set? symbol (symbols rb)) (cons 1 (encode-symbol symbol rb)))
        (else (error "Unknown symbol"))))))
  
    
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
        
;-------

(define (gen-huffman-tree pairs)
  (succ-merge (make-set-leaf pairs)))

(define (succ-merge set)
  (if (null? (cdr set))
      (car set)
      (succ-merge (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set)))))

;-------


(define a '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define song
'(
GET A JOB
SHA NA NA NA NA NA NA NA NA
GET A JOB
SHA NA NA NA NA NA NA NA NA
WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
SHA BOOM
))   
    
(define tree (gen-huffman-tree a))
tree
"---"
(define e (encode song tree))
e
(length e)
(* 3 (length song))
(decode e tree)
