;; A set is a data structure where all elements of the set
;; are unique and the following operations can be performed
;; on the set: union-set, intersection-set, adjoin-set and
;; element-of-set

;; A set can be defined based on the operations that can be
;; performed on the set. The underlying representation of the
;; data structure can vary


;; Set as unordered list

;; Set operations can be represented as

(define (element-of-set) x set
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set x set)
      set
      (cons x set)))
