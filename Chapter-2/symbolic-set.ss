;; A set is a data structure where all elements of the set
;; are unique and the following operations can be performed
;; on the set: union-set, intersection-set, adjoin-set and
;; element-of-set

;; A set can be defined based on the operations that can be
;; performed on the set. The underlying representation of the
;; data structure can vary


;; Set as unordered list

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set x (cdr set)))))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons
          (car set1)
          (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set
       (cdr set1)
       (adjoin-set (car set1) set2))))


;; Set assumed to contain distinct elements

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


;; Set assumed to contain duplicates

(define (adjoin-set-duped x set)
  (cons x set))


;; Set as ordered list

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


(define (ajoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else
         (cons
          (car set)
          (adjoin-set x (cdr set))))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2)))
               ((< x1 x2)
                (intersection-set (cdr set1) set2))
               ((> x1 x2)
                (intersection-set set1 (cdr set2))))))))


(define (union-set set1 set 2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (union-set (cdr set1)
                    (ajoin-set (car set1) set2)))))


;; Defining set representation as trees


;; Selectors

(define (entry tree) (car tree))


(define (left-branch tree) (cadr tree))


(define (right-branch tree) (caddr tree))


;; Constructor

(define (make-tree entry left right) (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= (entry set) x) true)
        ((< (entry set) x)
         (element-of-set? x (left-branch set)))
        ((> (entry set) x)
         (element-of-set? x (right-branch set)))))


(define (ajoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry x)) set)
        ((< x (entry x))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; Tree to list

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2)))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree-list-1 (left-branch tree))
       (cons
        (entry tree)
        (tree-list-1 (right-branch tree))))))


(define (list->tree-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; Tree representations

;; List representation 1 : 1 3 5 7 9 11
;; List representation 2 : 1 3 5 7 9 11

;; The second version has a complexitiy of O(n) since
;; each element of the list is traversed once followed by a simple
;; cons operation of that takes O(1) time

;; The first operations call the append operation as defined above
;; since append is proportional to the size of the list, and the list
;; that is provided as input is about half the size of the total elements
;; and this operation is called recursively, the complexity in the second case
;; would be O(nlogn)



;; List to balanced tree

(define (list->tree elements)
  (partial-tree elements (length elements))
  (define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
          (let ((left-result (partial-tree elts left-size)))
            (let ((left-tree (car left-result))
                  (non-left-elts (cdr left-result))
                  (right-size (- n (+ left-size 1))))
              (let ((this-entry (car non-left-elts))
                    (right-result (partial-tree (cdr non-left-elts)
                                                right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry left-tree right-tree)
                        remaining-elts)))))))))

;; a.
;; Example:
;; Make balanced tree from '(1 3 5 7 9 11)
;;
;;
