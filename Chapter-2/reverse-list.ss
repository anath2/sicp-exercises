;; Reverse list

;; Reverses list elements 

(define (reverse-list l)
  (if (null? (cdr l))
      l
      (cons (reverse-list (cdr l)) (car l))))

;; Deep reverse

;; Reverses the elements of the list along with all
;; sublists

(define (deep-reverse-list l)
  (if (null? l) l
      (cons (deep-reverse-list (cdr l))
	    (if (not (pair? (car l)))
		(car l)
		(deep-reverse-list (car l))))))



