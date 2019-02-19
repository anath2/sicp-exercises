;; Complex numbers as defined and represented
;; using a rectangular representation and polar representation


;; Constructor

((define make-from-real-img real img) (cons real img))

((define make-from-mag-ang mag ang) (cons mag ang))


;; Selectors

(define (real-part z) (car z))

(define (img-par z) (cadr z))

(define (magnitude z) (car z))

(define (angle z) (cadr z))
