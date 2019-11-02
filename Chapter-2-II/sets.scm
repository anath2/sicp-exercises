;; Representing sets and set operations
;; ------------------------------------

;; Representation as unordered list
;; Note, that for operations of union and intersection
;; for every element of the set, an operation is performed to
;; check if it is an element of the other set. This has O(n) complexity
;; Hence, the complexity for union and intersection would be the product
;; of lengths of two sets.
;; O(n * m)


(define (element-of-set-u? x set)
  (cond ((null? set) #f)
	((equal? (car set) x) #t)
	(else (element-of-set-u? x (cdr set)))))


(define (adjoin-set-u x set)
  (if (element-of-set-u? x set)
      set
      (cons x set)))


(define (union-set-u set-a set-b)
  (cond ((null? set-a) set-b)
	((element-of-set-u? (car set-a) set-b)
	 (union-set-u (cdr set-a) set-b))
	(else (cons (car set-a)
		    (union-set-u (cdr set-a) set-b)))))
	

(define (intersection-set-u set-a set-b)
  (cond ((or (null? set-a) (null? set-b)) '())
	((element-of-set-u? (car set-a) set-b)
	 (cons (car set-a)
	       (intersection-set-u (cdr set-a) set-b)))
	(else (intersection-set-u (cdr set-a) set-b))))


;; Representing set as ordered list
;; Here, for operations of intersection and union, the complexity would
;; O(m + n) where m and n are the sizes of the two sets respectively

(define (element-of-set-o? x set)
  (cond ((null? set) #f)
	((< x (car set)) #f)
	((= x (car set)) #t)
	(else (element-of-set-o? x (cdr set)))))


(define (adjoin-set-o x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set)
		    (adjoin-set-o x (cdr set))))))


(define (union-set-o set-a set-b)
  (cond ((null? set-a) set-b)
	((null? set-b) set-a)
	((< (car set-a) (car set-b))
	 (cons (car set-a)
	       (union-set-o (cdr set-a) set-b)))
	(else (cons (car set-b)
		     (union-set-o set-a (cdr set-b))))))


(define (intersection-set-o set-a set-b)
  (cond ((or (null? set-a) (null? set-b)) '())
	((= (car set-a) (car set-b))
	 (cons (car set-a)
	       (intersection-set-o (cdr set-a) (cdr set-b))))
	((< (car set-a) (car set-b))
	 (intersection-set-o (cdr set-a) set-b))
	(else (intersection-set-o set-a (cdr set-b)))))


;; Representing set as a Binary search tree
;; A binary search tree is represented such that the left child is smaller than the
;; root and right child is larger than the root element

;; defining tree representation

;; constructor
(define (make-tree entry left right)
  (list entry left right))


;;Selectors
(define (entry tree) (car tree))


(define (left-branch tree) (cadr tree))


(define (right-branch tree) (caddr tree))


(define (element-of-set-t? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set)) (element-of-set-t? x (left-branch set)))
	((> x (entry set)) (element-of-set-t? x (right-branch set)))))


(define (adjoin-set-t x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set-t x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)		    
		    (adjoin-set-t x (right-branch set))))))


;; Tree, ordered list conversion
(define (tree->o-list-v1 tree)
  ;;If we assume append has linear time
  ;; complexity (Since each element has to be selected for
  ;; append, the overall complexity here is O(nlogn)
  
  (if (null? tree)
      '()
      (append (tree->o-list-v1 (left-branch tree))
	      (cons (entry tree)
		    (tree->o-list-v1 (right-branch tree))))))


(define (tree->o-list-v2 tree)
  ;; Procedure takes O(n) time since, cons is O(1)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))


(define (o-list->tree o-list)
  (define (partial-tree ls n)
    (if (= n 0)
	(cons '() ls)
	(let ((left-size (quotient (- n 1) 2)))
	  (let ((left-result (partial-tree ls left-size)))
	    (let ((left-tree (car left-result))
		  (non-left-ls (cdr left-result))
		  (right-size (- n (+ 1 left-size))))
	      (let ((this-entry (car non-left-ls))
		    (right-result (partial-tree (cdr non-left-ls) right-size)))
		(let ((right-tree (car right-result))
		      (remaining-ls (cdr right-result)))
		  (cons (make-tree this-entry left-tree right-tree)
			remaining-ls))))))))
  (car (partial-tree o-list (length o-list))))


(define (union-set-t set-a set-b)
  (let ((l1 (tree->o-list set-a))
	(l2 (tree->o-list set-b)))
    (union-set-o l1 l2)))


(define (intersection-set-t set-a set-b)
  (let ((l1 (tree->o-list set-a))
	(l2 (tree->o-list set-b)))
    (intersection-set-o l1 l2)))


;; Sets for information retrieval
;; A set can be used as a data structure for storing records
;; the internal representation of the set dictates how fast/slow
;; can the information contained in the set be retrieved


(define (lookup-o given-key set-of-records)
  ;; Consider the set of records to a be an unordered set
  ;; Each record in the set is identified by a unique key
  ;; used to retrieve the rest of the rows in the record  
  (cond ((null? set-of-records) #f)
	((equal? given-key (key (car set-of-records))) (car set-of-records))
	(else (lookup-o given-key (cdr set-of-records)))))


(define (lookup-t given-key set-of-records)
  ;; Similiar to the above, except the records are arranged in
  ;; binary tree using numerical keys
  (cond ((null? set-of-records) #f)
	((= given-key (key (entry set-of-records))) (entry set-of-records))
	((< given-key (key (entry set-of-records))) (lookup-t given-key (left-branch set-of-records)))
	((> given-key (key (entry set-of-records))) (lookup-t given-key (right-branch set-of-records)))))
