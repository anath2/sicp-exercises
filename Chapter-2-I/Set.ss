#lang racket

;; We are going to represent sets using different set of data structures

;; UNORDERED LIST

;; Assuming sets do not contin duplicates

;; Operations on sets

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (let ((f (car set1)))
        (union-set (cdr set1)
                   (if (element-of-set? f set2)
                       set2
                       (cons f set2))))))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                set2)))
        (else (intersection-set (cdr set1) set2))))
        

(define (ajoin-set set x)
  (if (element-of-set? set x)
      set
      (cons x set)))
      
      
(define (element-of-set? set x)
  (cond ((null? set) #f)
        ((eq? (car set) x) #t)
        (else (element-of-set? (cdr set) x))))


;; Representing sets that may contain duplicates

(define (intersection-set-2 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set-2? set2 (car set1))  ;; Ensures that least of the number of duplicates among sets are selected
         (cons (car set1)
               (intersection-set-2 (cdr set1)
                                   (remove-set-element set2 (car set1)))))
        (else (intersection-set-2 (cdr set1) set2))))


(define (remove-set-element set x)
  (define (iter acc rest)
    (cond ((null? rest) acc)
          ((eq? (car rest) x) (append acc (cdr rest)))
          (else (iter (cons (car rest) acc) (cdr rest)))))
  (iter '() set))


(define (union-set-2 set1 set2)
  (append set1 set2))


(define element-of-set-2?
  element-of-set?)  


(define (adjoint-set-2 set x)
  (cons x set))


;; ORDERED LIST

