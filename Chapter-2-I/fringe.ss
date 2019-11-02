;; Fringe takes as an argument a tree and returns a list
;; whose elements are all the leaves of the tree arranged in 
;; left to right order

(define (fringe l)
  (cond ((null? l) '())
	((not (pair? l)) (list l))
	(else (append (fringe (car l))
		      (fringe (cdr l))))))


