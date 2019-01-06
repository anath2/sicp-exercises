;; A set is a data structure where all elements of the set
;; are unique and the following operations can be performed
;; on the set: union-set, intersection-set, adjoin-set and
;; element-of-set

;; A set can be defined based on the operations that can be
;; performed on the set. The underlying representation of the
;; data structure can vary


;; Set as unordered list

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set x (cdr set)))))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons
          (car set1)
          (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set
       (cdr set1)
       (adjoin-set (car set1) set2))))


;; Set assumed to contain distinct elements

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


;; Set assumed to contain duplicates

(define (adjoin-set-duped x set)
  (cons x set))


;; Set as ordered list

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


(define (ajoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else
         (cons
          (car set)
          (adjoin-set x (cdr set))))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2)))
               ((< x1 x2)
                (intersection-set (cdr set) set2))
               ((> x1 x2)
                (intersection-set set1 (cdr set2))))))))
