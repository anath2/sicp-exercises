;; Ex 2.24

;; Dispatch on get record per each company division

;; a) Get record
;; Explain how individual division's files should be structured

;; The dispatch table consists of division names as rows and corresponding
;; retrieval procedures as columns

;; Each division consists of a single file tagged by name of division
;; Each employee record in the file are further tagged by the name
;; of the employee

;; Consider the following example

(define (install-division-1-package)
  (define (get-record name file)
    (cond ((null? file) (error "Record could not be found"))
          ((eq? name (get-name (cadr file))) (cadr file))
          (else (get-record name (cdr file)))))

  (define (get-salary name file)
    (cond ((null? file) (error "no result"))
          ((eq? name (get-name (cadr file))) (cadr (cadr file)))
          (else (get-salary name (cdr file)))))

  (define (get-name record)
    (car record))

  (put 'division-1 get-record)
  (put 'division-1 get-name)
  (put 'division-1 get-salary)
  'done)

;; Generic procedures

(define (get-record name file)
  (apply-generic 'get-record name file))


(define (get-salary name file)
  (apply-generic 'get-salary name file)


(define (apply-generic op name file)
  (let ((div-name (type-tag file)))
    (let ((func (get op div-name)))
      (if func
          (func name file)
          (error "Procedure not defined")))))


(define (type-tag file)
  (car file))


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


;; c) find-employee-record
;; Implement headquarter's find-employee record procedure given
;; the following arguments :
;; 1. Employee's name
;; 2. List of all division files

(define (find-employee-record employee-name div-flist)
  (if (null? div-flist)
      'nil
      (let ((curr-file (car div-flist)))
        (if (get-record employee-name curr-file)
            (get-record employee-name curr-file)
            (find-employee-record employee-name (cdr div-flist))))))


;; d) If insatiable enterprises takes over a new company, the company records
;; have to tagged by the previous name of the (new division). within each record
;; employee records have to be tagged with the name of employee so that
;; the records are searchable