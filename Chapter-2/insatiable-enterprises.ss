;; Ex 2.24

;; Dispatch on get record per each company division

;; a) Get record
;; Explain how individual division's files should be structured
;;
;; Each division has a single personnel file contains records
;; keyed on employee names
;; Each file is arranged differently by each invidividual
;; division. Each record is a set keyed by fields such as address,
;; salary etc

;; Each division's retrieval methods should be tagged with name
;; of the division
;; Also, each record file should have the name of the division

(define (get-record employee-name)
  (if (null? employee-name)
      (error "Employe name has to be non null" employee-name)
      (let div-name (get '(employee-name) 'division)
        ;; Dispatch on employee-retrieval for the department
        ((get 'get-record '(div-name) employee-name)))))
