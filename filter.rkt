#lang racket

(define (unique ls ll)
(if (null? ls )
      ll
     (reverse (unique (cdr ls)  (set-add ll (car ls))))
  )
)

;(require racket/set)
(unique '(1 2 3 3 3 4 4 4 4 4 5 55) '(8 ))
