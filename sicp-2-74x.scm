#lang sicp

; division-file

; -- selectors
; first-file-item file
; rest-file-items file

; file-item
;   -- selectors
;   file-item-key item
;   file-item-record item
;   -- constructor
;   make-file-item key record
   
; employee-record

;   -- selectors
;   first-record-item record
;   rest-record-items record

;   record-item
;   -- selectors
;   record-item-key item
;   record-item-value item
;   -- constructor
;   make-record-item key value

(define (first-file-item file)
  (apply-generic 'first-file-item file))

(define (rest-file-items file)
  (apply-generic 'rest-file-items file))

(define (file-item-key item)
  (apply-generic 'file-item-key item))

(define (file-item-record item)
  (apply-generic 'file-item-record item))

(define (first-record-item record)
  (apply-generic 'first-record-item record))

(define (rest-record-items record)
  (apply-generic 'rest-record-items record))

(define (record-item-key item)
  (apply-generic 'record-item-key item))

(define (record-item-value item)
  (apply-generic 'record-item-value item))

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

(define (install-d1-package)
  (define (first-file-item file)
    (car file))

  (define (rest-file-items file)
    (cdr file))

  (define (file-item-key item)
    (car item))

  (define (file-item-record item)
    (cdr item))

  (define (first-record-item record)
    (car record))

  (define (rest-record-items record)
    (cdr record))

  (define (record-item-key item)
    (car item))

  (define (record-item-value item)
    (cdr item))
  
  (put 'first-file-item 'd1 first-file-item)
  (put 'rest-file-items 'd1 rest-file-items)
  (put 'file-item-key 'd1 file-item-key)
  (put 'file-item-record 'd1 file-item-record)
  (put 'first-record-item 'd1 first-record-item)
  (put 'rest-record-items 'd1 rest-record-items)
  (put 'record-item-key 'd1 record-item-key)
  (put 'record-item-value 'd1 record-item-value))

;---

