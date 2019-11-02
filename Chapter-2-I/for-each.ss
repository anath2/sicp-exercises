;; Implement for-each, for-each applies a function to each element of the list 
;; instead of returning a list. This is especially useful for performing an action
;; on each element of the list. Ex: printing elements of the list

(define (for-each f items)
  (cond ((null? items) #t)
	(else 
	  (f (car items))
	  (for-each f (cdr items)))))

