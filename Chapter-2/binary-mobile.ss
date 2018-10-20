;; Construct a binary mobile which is represented by a binary 
;; tree with variable rod lengths and weights

;; Data constructor

(define (make-mobile left right)
  (list left right))

;; Branch constructor

(define (make-branch len structure)
  (list len structure)

;; Selectors

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; Functions

