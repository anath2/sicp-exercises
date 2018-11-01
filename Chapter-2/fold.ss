;; Define left fold and right fold functions in lisp

;; Accumulate is same as fold right since elements are
;; folded from the right

;; Fold right

(define (foldr op inital sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (foldr op initial (cdr sequence)))))

;; Fold left

(define (foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;; Operations

(fold-right / 1 (list 1 2 3)) ;; 3 / 2

(fold-left / 1 (list 1 2 3)) ;; 1 / 3

(fold-right list '() (list 1 2 3)) ;; (list 1 2 3 '())

(fold-left list '() (list 1 2 3)) ;; (list (list (list '() 1) 2) 3)

;; Operation must be commutative to fold left and fold right must be same      

