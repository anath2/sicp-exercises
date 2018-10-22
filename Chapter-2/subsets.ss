;; Represent all subsets of a set (set being represented by a list)
;; as a list of sublists 

;; Explanation: The subsets can be reprsented as the combination 
;; of all the subsets of elements except the first element
;; and all the subsets of elements except the first element plus the first elemnt


(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (subset)
			    (cons (car s) subset))
			  rest)))))

