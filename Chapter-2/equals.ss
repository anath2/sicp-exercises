;; Define an equals method that takes two lists containing
;; symbols and tells whether the two are equal or not


(define (custom-equals? list-1 list-2)

	(cond	((and (null? list-1) (null? list-2)) #t)
  			((not (= (length list-1) (length list-2))) #f)
				((not (eq? (car list-1) (car list-2))) #f)
				(else (custom-equals? (cdr list-1) (cdr list-2)))))
