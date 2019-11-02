;; Square all elements of a tree structure


;; Implemented by squaring all elements of the tree
(define (square-tree t)
  (cond ((null? t) '())
	((not (pair? t)) (* t t))
	(else (cons (square-tree (car t))
		    (square-tree (cdr t))))))

;; Implemented by treating the tree as sequence of
;; subtrees and mapping over individual subtrees

(define (square-tree-map t)
  (map (lambda (subtree) 
	 (if (pair? subtree) (square-tree-map subtree)
	     (* subtree subtree))
       ) t))	
