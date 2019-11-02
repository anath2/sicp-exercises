;; Construct a binary mobile which is represented by a binary 
;; tree with variable rod lengths and weights

;; Data constructor

(define (make-mobile left right)
  (list left right))

;; Branch constructor

(define (make-branch len structure)
  (list len structure))

;; Selectors

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (cadr branch))

;; Mobile weight

(define (mobile? s) (pair? s))

(define (weight? s)
  (and (not (mobile? s))
       (number? s)))

(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch))
	(if (weight? structure)
	    structure
	    (mobile-weight structure)))))

;; Balanced Mobile

(define (mobile-balanced? mobile)
  (define (branch-torque branch)
    ( * (branch-length branch)
	(branch-weight branch)))
  (define (balanced-branch? branch)
    (let ((structure (branch-structure branch)))
      (or (weight? structure)
	  (mobile-balanced? structure))))

  (let ((left (left-branch mobile))
	(right (right-branch mobile)))
    (and (branch-balanced? left)
	 (branch-balanced? right)
	 (= (branch-torque left)
	    (branch-torque right)))))



