;; Differentiation using symbols

;; Formulas implemented here include
;; 1. diff of constant
;; 2. diff of (u + v)
;; 3. diff of (u . v)


;; Other formulae:

;; (variable? e) : Is e a variable?
;; (same-variable? v1 v2) : Are v1 and v2 equivalent
;; (sum? e) : Is e a sum
;; (addend e) : Addend of sum e
;; (augend e) : Augent of sum e
;; (make-sum a1 a2) : Construct the sum of a1 and a2

;; (product? e) : Is e a product
;; (multiplier e) : Multiplier of the product e
;; (multiplicand e) : Multiplicand of the product e
;; (make-product m1 m2) : Construct the product of m1 and m2


;; Representation

;; In our representation, variables are represented as variables
;; And expression are represented by lists such as :
;; ax + b => (+ (* a x) b)
;; Hence we cand define the above functions as following

(define (variable? e) (symbol? e))


(define (same-variable? v1 v1)
  (and
   (variable? v1)
   (variable? v2)
   (eq? v1 v2)))


(define (=number? exp num)
  (and (number? exp) (= exp num)))


;; Addition rule of derivative

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


(define (sum? e)
  (and (pair? e)
       (eq? (car e) '+)))


(define (addend e) (cadr e))


(define (augend e)
  (let (cddr (cddr e) a)
    (if (= (length a) 1)
        (car a)
        (make-sum-list a))))


(define (make-sum-list l)
  (if (= (length l) 2)
      (list '+ (car l) (cadr l))
      (make-sum (car l) (make-sum-list (cdr l)))))


;; Product rule of derivative

(define (make-product p1 p2)
  (cond ((or (=number? p1 0) (=number? p2 0)) 0)
        ((=number? p1 1) p2)
        ((=number? p2 1) p1)
        (and (number? p1) (number? p2) (* p1 p2))
        (else (list '* p1 p2))))


(define (product? e)
  (and
   (pair? e)
   (eq? (car e) '*)))


(define (multiplier e) (cadr e))


(define (multiplicand e) (caddr e))


;; Exponentiation rule

(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else
          (list '** base exponent))))


(define (exponentiation? e)
  (and (pair? e)
       (eq (car e) '**)
       (= (length e) 3)))


(define (base e) (cadr e))


(define (exponent e) (caddr e))


;; Deriv
;; Take derivative based on the functions described
;; above

(define (derive exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum
          (deriv (augend exp) var)
          (deriv (addend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp) (deriv (multiplicand exp) var))
          (make-product
           (multiplicand exp) (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-exponent (base exp) (- (exponent exp) 1))
          (deriv (base exp))))
        (else
         (error "Unknow expression given"))))
