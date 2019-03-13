;; Ex 2.24

;; Dispatch on get record per each company division

;; a) Get record
;; Explain how individual division's files should be structured
;;
;; Each division has a single personnel file contains records
;; keyed on employee names
;; Each file is arranged differently by each invidividual division.
;; Each record is a set keyed by fields such as address,
;; salary etc

;; Each division's retrieval methods should be tagged with name of the division
;; Also, each record file should be pair with employee-name and division-name

(define (get-record employee-name div-name)
  (if (null? employee-name)
      (error "Employe name has to be non null" employee-name)
      ;; Dispatch on employee-retrieval for the department
      ((get 'get-record '(div-name)) employee-name)))

;; b) get-salary
;; Each division's get salary method should be tagged with the name of the
;; division

(define (get-salary employee-record div-name)
  ((get 'get-salary '(div-name)) employee-record))
