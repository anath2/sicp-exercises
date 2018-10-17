;; Square a list of numbers 
;; There are two ways for calculating square of a series

;; 1.

(define (square-list-1 l)
  (if (null? l)
      '()
      (cons 
	(* (car l) (car l)) 
	(square-list-1 (cdr l)))))

;; 2.

(define (square-list-2 l)
  (map (lambda (x) (* x x))
       l))



