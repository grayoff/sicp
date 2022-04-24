#lang sicp

(define (get-record name file)
  (apply-generic 'get-record (attach-tag 'string name) file))

(define (get-salary record)
  (apply-generic 'get-salary record))

(define (find-employee-record name files)
  (if (null? files)
      nil
      (let ((r (get-record name (car files))))
        (if (null? r)
        (find-employee-record name (cdr files))
        r))))
        

;---

(define table '())

(define (put op type item)
  (set! table (cons (cons op (list type item)) table)))
  
(define (get op type)
  (define (get-from-table t)
    (if (eq? t '())
        (error "No op found" op type)
        (if (and (eq? (caar t) op)
                 (equal? (cadar t) type))
            (caddar t)
            (get-from-table (cdr t)))))
  (get-from-table table))

(define (apply-generic op . args)
  (apply (get op (map type-tag args)) (map contents args)))
  
;---

(define (attach-tag type-tag contents)
 (list type-tag contents))

(define (type-tag datum)
 (if (pair? datum)
     (car datum)
     (error "Bad tagged datum" datum)))

(define (contents datum)
 (if (pair? datum)
     (cadr datum)
     (error "Bad tagged datum" datum)))

;---

(define d1 (attach-tag 'd1 '(100)))

(define (install-d1-package)
  (define (get-record name file)
    (attach-tag 'd1-rec (cons name file)))

  (define (get-salary record)
    (cadr record))

  (put 'get-record '(string d1) get-record)
  (put 'get-salary '(d1-rec) get-salary))

;---

(define d2
  (attach-tag 'd2
   (list
     (attach-tag 'd2-rec '(("name" "Ivan") ("address" "Moskow") ("salary" 1000) ("title" "Programmer")))
     (attach-tag 'd2-rec '(("name" "Boris") ("address" "StPeterburg") ("salary" 950) ("title" "Programmer")))
     (attach-tag 'd2-rec '(("name" "Peter") ("address" "Moskow") ("salary" 1000) ("title" "Programmer")))
     (attach-tag 'd2-rec '(("name" "Alex") ("address" "Moskow") ("salary" 800) ("title" "Manager")))
     )))

(define (install-d2-package)
  (define (get-record name file)
    (cond ((null? file) nil)
          ((eq? (cadr (caadar file)) name) (car file))
          (else (get-record name (cdr file)))))

  (define (get-salary record)
    (cadr (caddr record)))

  (put 'get-record '(string d2) get-record)
  (put 'get-salary '(d2-rec) get-salary))

;---

(define d3
  (attach-tag 'd3
   (list
     (attach-tag 'd3-rec '("Bill" "Novigrad" 700 "Programmer"))
     (attach-tag 'd3-rec '("John" "Novigrad" 700 "Programmer"))
     (attach-tag 'd3-rec '("Nick" "Novigrad" 700 "Programmer"))
     (attach-tag 'd3-rec '("Greg" "Novigrad" 500 "Manager"))
     )))

(define (install-d3-package)
  (define (get-record name file)
    (cond ((null? file) nil)
          ((eq? (caadar file) name) (car file))
          (else (get-record name (cdr file)))))

  (define (get-salary record)
    (caddr record))

  (put 'get-record '(string d3) get-record)
  (put 'get-salary '(d3-rec) get-salary))

;---

(install-d1-package)
(install-d2-package)
(install-d3-package)

d1
(get-record "Andrew" d1)
(get-salary (get-record "Andrew" d1))

d2
(get-record "Peter" d2)
(get-salary (get-record "Peter" d2))
(get-record "Willem" d2)

d3
(get-record "Greg" d3)
(get-salary (get-record "Greg" d3))

(find-employee-record "Boris" (list d2 d3))
(find-employee-record "Nick" (list d2 d3))
(find-employee-record "Nope" (list d2 d3))