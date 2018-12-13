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