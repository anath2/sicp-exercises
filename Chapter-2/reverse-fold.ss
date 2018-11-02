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

;; Reverse using foldr

(define (reverse-foldr sequence)
  (define nil '())
  (foldr (lambda (elem already-reversed)
	   (append already-reversed (list elem)))
	 nil sequence))

;; Reverse using foldl

(define (reverse-foldl sequence)
  (define nil '())
  (foldl (lambda (result first) (cons first result))
	 nil sequence))



	   	

