;; procedure, that takes an arbitrary number of arguments
;; and returns the list of argument with the same odd-even
;; parity as the first argument

;; Filter elements by parity

(define (filter-by-parity l parity)
    (cond ((null? l)
	   '())
	  ((= (remainder (car l) 2.0) parity)
	   (cons (car l) (filter-by-parity (cdr l) parity)))
	  (else (filter-by-parity (cdr l) parity))))

;; Filter args by parity

(define (same-parity . args)
    (if (null? args)
	'()
	(filter-by-parity args (remainder (car args) 2.0))))
