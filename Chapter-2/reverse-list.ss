;; Reverse list

;; Reverses list elements 

(define (reverse-list l)
  (if (null? (cdr l))
      l
      (cons (reverse-list (cdr l)) (car l))))

