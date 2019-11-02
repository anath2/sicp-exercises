;; Use count leaves and use accumulate to count the leaves of 
;; tree

;; Define accumulation

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;; Count tree leaves using accumulation
;; Use map to represent each '1' for each leaf of 
;; of the tree. Empty lists are represented by 
;; '0'. The map function here, flattens the tree in way that all
;; The non leaf nodes represent the count for the nodes under it

(define (acc-count-leaves t)
  (accumulate 0 +
	      (map 
		(lambda (tree) 
		  (cond ((null? tree) 0)
			((pair? tree) 1)
			(else (acc-count-leaves tree)))))))




