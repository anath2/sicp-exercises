;; memq
;; Returns the list if the symbod is present in
;;  the list

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


;; Evaluate the result in case of each below

(list 'a 'b 'c) ;; '(a b c)

(list (list 'george)) ;; '((george))

(cdr '((x1 x2) (y1 y2))) ;; '((y1 y2))

(cadr '((x1 x2) (y1 y2))) ;; '(y1 y2)

(pair? (car '(a short list))) ;; #f

(memq 'red '((red shoes) (blue socks))) ;; false

(memq 'red '(red shoes blue socks)) ;; '(red shoes blue socks)


;; Define an equals method that takes two lists containing
;; symbols and tells whether the two are equal or not

(define (custom-equals? list-1 list-2)

	(cond	((and (null? list-1) (null? list-2)) #t)
  			((not (= (length list-1) (length list-2))) #f)
				((not (eq? (car list-1) (car list-2))) #f)
				(else (custom-equals? (cdr list-1) (cdr list-2)))))


;; Evaluate the following:

(car ''abracadabra) ;; 'quote

;; Reason:
;; This is because, ''abracadabra is equivalent to (quote (quote abracadabra))
;; Since, the function quote quotes the next expression,
;; (quote (quote something)) => '(quote something)
;; calling car on which would yield quote
