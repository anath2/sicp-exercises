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


(define (make-sum a1 a2)
  (list '+ a1 a2))


(define (make-product p1 p2)
  (list '* p1 p2))


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
        (else
         (error "Unknow expression given"))))
