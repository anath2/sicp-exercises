;; Implement reverse list using fold left and fold right

;; foldr

(define (foldr op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (foldr op initial (cdr sequence)))))

;; foldl

(define (foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest)) 
	      (cdr rest))))
  (iter initial sequence))



