;; Operations on matrices and vectors


;; Accumulate

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;; Dot product

;; Scheme map takes multiple lists and a operation and 
;; applies the operation on each element of the list
;; element by element

(define (dot-product v w)
  (accumulate + 0 (map * v w)))



