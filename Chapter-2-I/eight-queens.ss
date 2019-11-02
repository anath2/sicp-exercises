#lang racket


;; N queens problem tries to solve the problem of arranging N queens on a board of size N
;; in a way that no two queens check each other\


;; Utility functions

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start
            (enumerate-interval (+ start 1)
                                end))))

(define (accumulate proc init seq)
  (if (null? seq)
      init
      (proc (car seq)
            (accumulate proc init (cdr seq)))))
      

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


;; Board utilities

;; A position on the board is represented by a pair of x, y with x representing
;; row and y column, starting position being 1 and ending being the size of the board

(define empty-board '())

;; Adds a new position to board

(define (adjoin-position new-row new-col position-list)
  (cons (cons new-row new-col)
        position-list))

;; Checks if the position is safe

(define (safe? new-column position-list)
  ;; Considering the first position as the trial position
  ;; Since this function is called recursively, the subsequent
  ;; accumulation would only contain safe positions

  (let ((trial-row (car (car position-list)))
        (trial-col (cdr (car position-list)))
        (rest (cdr position-list)))

    (accumulate (lambda (position result)
                  (let ((curr-row (car position))
                        (curr-col (cdr position)))
                   
                    (and (not
                          (or (= curr-row trial-row)
                              (= (- curr-row curr-col)
                                 (- trial-row trial-col))
                              (= (+ curr-row curr-col)
                                 (+ trial-row trial-col))))
                         result)))
                #t
                rest)))
        
;; n-queen problem

(define (n-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; n-queen (Buggy)

(define (n-queens-2 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
    (queen-cols board-size))