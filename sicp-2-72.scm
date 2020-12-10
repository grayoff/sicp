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
  
    
; most frequent O(n)
; least frequent O(n**3/2)