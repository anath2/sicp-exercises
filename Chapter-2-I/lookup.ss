;; A look structure is used to lookup values
;; using a key. Assuming structure is a set
;; the following operations can be performed


;; Unordered set

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))


;; Binary tree

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (left set-of-records)))
         (lookup given-key (left set-of-records)))
        (else (lookup given-key (right set-of-records)))))
