;; last-pair.ss

;; From a list, get the list containing only the last element of the list
;; as a pair

(define (last-pair items)
  (if (null? (cdr items))
      (list (car items))
      (last-pair (cdr items))))



