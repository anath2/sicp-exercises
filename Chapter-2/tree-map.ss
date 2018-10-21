;; A procedure that maps a function onto a tree

;; f being an arbitrary function. We traverse the tree in a 
;; pre-order fashion

(define (tree-map f t)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map f subtree)
	     (f subtree)))
       t))

