
;; An alternative representation of a pair

;; Defining cons

(define (cons x y)
  (lambda (m) (m x y)))

;; Define car - Get first element of the pair

(define (car z)
  (z (lambda (p q) p)))

;; Define cdr - Get the second element of the pair

(define (cdr z)
  (z (lambda (p q) q)))

;; Substitution proof for car

;; (car (cons x y))

;; -> ((z (lambda (p q) p)) (cons x y))

;; -> ((z (lambda (p q) p)) (lambda (m) (m x y)))

;; -> ((lambda (m) (m x y)) (lambda p q) p)

;; -> p
