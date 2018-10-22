;; Represent all subsets of a set (set being represented by a list)
;; as a list of sublists

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (subset)
			    (cons (car s) subset))
			  rest)))))

