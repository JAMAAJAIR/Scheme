#lang racket
(require racket/trace)

(define (mdulo a b)
        (if ( < (- a b ) 0)
          a
          (modulo (- a b) b )))

(define (length_list_acc length ls)
        (if (null? ls)
        length
        (length_list_acc (+ 1 length) (cdr ls)))
)
;----------------------- EXO 1 ---------------------------
; calculate pgcd of two numbers with euclid algorithm
(define (pgcd a b)
    (cond ( (= a  0 ) b)
          ( (= b  0 ) a)
          ( (> a  b ) (pgcd b (modulo a b)))
          ( (< a  b ) (pgcd a (modulo b a)))
))

;(pgcd 5 60)

;------------------------- EXO 2--------------------------

; sum_lis

; without accimilator
(define (sum_lis ls)
        (if (null? ls )
        0
        (+ (car ls) (sum_lis (cdr ls))))
)

;(sum_lis '(1 2 3))
; sum_lis with accimilator

(define (sum_lis_acc some ls)
        (if (null? ls)
        some
        (sum_lis_acc (+ some (car ls)) (cdr ls)))
)
;(sum_lis_acc 0 '(1 2 3))
;
(define (suffix n ls)
        (if (= n (length_list_acc 0 ls))
        ls
        (suffix n  (cdr ls)))
)

;(trace suffix)
;(suffix 2 '(1))


(define (bigger n ls)
        (cond ((null? ls) '())
              ((>= (car ls) n) (cons (car ls) (bigger n (cdr ls))))
              ((< (car ls) n) (bigger n (cdr ls)))
        )
)

;(trace bigger)
;(bigger 5 '(1 4 2 9 8 7 6 2 3 1))

(define (zip ls lr)
        (cond ((and (null? ls) (null? lr)) '())
              ((and (null? ls) (not (null? lr)) lr))
              ((and (null? lr) (not (null? ls)) ls))
              ((cons (cons (car ls) (car lr)) (zip  (cdr ls) (cdr lr))
              ))
        )
)

;(zip '(1 2 3) '(a b c))
(define (min_list min ls)
        (cond ((null? ls) min)
              ((>= (car ls) (cadr ls)) (min_list (cadr ls) (cdr ls) ))
              ((<  (car ls) (cadr ls)) (min_list (car ls ) (cdr ls) ))
        )
)

;(min_list 0 '(151 34 19 27 8 26 ))

(define (index-of n ls pos)
        (cond ((null? ls )  #f)
              ((equal? (car ls) n) pos)
              (not(equal? (car ls) n ) (index-of n (cdr ls) (+ pos 1)))
        )
)
(trace index-of)
(index-of 4 '(1 2 3 4) 0)
