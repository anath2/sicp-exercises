;; 2.52 Combining dtypes

;; Defining operations across type boundaries

;; to be included in the complex package

(define (add-complex-to-schemenum z x)
  (make-from-real-img (+ (real-part z) x)
                      (img-part z)))

(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))


;; Using coersion

(define (scheme-number->complex n)
  (make-complex-from-real-img (contents n) 0))

;; Then put this in a special coersion table
(put-coersion 'scheme-number 'complex scheme-number->complex)


;; Apply generic that utilizes the coersion table

(define (apply-generic args . op)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get type-tags op)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type-1 (car type-tags))
                    (type-2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coersion type1 type2))
                      (t2->t1 (get-coersion typ2 typ1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op (t2->t1 a1) a2))
                        (else
                         (error "No method for these types" (list op type-tags))))))
              (error "No methods for these types"
                     (list op type-tags)))))))


;;-------------------------

;; apply-generic
;; Generalized apply generic that handles coersion

;; Ex 2.81 :

(define (apply-generic args . op)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get type-tags op)))
      (if proc
          (apply proc (map contents args))
          (error "No method found for types -- APPLY-GENERIC"
                 (list op type-tags))))))


(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad data - TYPE-TAG", datum)))


(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad data -- CONTENTS" datum)))
