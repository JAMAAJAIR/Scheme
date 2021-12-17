#lang racket

;(define (append lst tail)
;  (if (null? lst)
;      tail
;      (cons (car lst)
;            (append (cdr lst)
;                    tail))))
(require racket/trace)
;(trace append)
;(append '(4 5 6) '(1 2 3))
( define ( sayit x )
          ( cond ((= x 1) 'un )
                  ((= x 2) 'deux )
                  ((= x 3) 'trois )
                  ((= x 4) 'quatre )
                  ((= x 5) 'cinq )
                  ( else 'inconnnu ) ) )

;( define roots( lambda ( a b c )
;
;                  (let (( delta (- (* b b ) (* 4 a c ) ) ) )
;                  ( cond ((< delta 0) '() )
;                  ((= delta 0) ( list (- (/ b (* 2 a ) ) ) ) )
;                  ( else ( list (- (/ (+ b ( sqrt delta ) ) (* 2 a ) ) )
;                  (- (/ (- b ( sqrt delta ) ) (* 2 a ) ) ) ) ) ) ) ) )

(define jair_resoudre_equ (lambda (a b c )
                  (let* ((delta (- (* b b) (* 4 a c))))
                  (cond ((zero? delta)
                        (list ( / (- 0  b) ( * 2 a)))
                       )
                       ((> delta 0)
                        (list (/ (- b (sqrt delta)) (* 2 a)) (/ (+ (- 0 b) (sqrt delta)) (* 2 a))))
                       ((< delta 0)
                        '())
                  ))))

(define (sum_int n)
        (if(zero? n)
        0
        (+ n (sum_int (- n 1)))))

(trace sum_int)
;(sum_int 4)


(define (countdown  ls n)
  (if (= 0 n)
  ls
  (countdown (cons n ls)(- n 1)))
)
;(countdown  '() 5)

(define (countup ls cpt n)
  (if (or (= cpt n) (= n 0))
  ls
  (countup (cons cpt ls) (+ cpt 1) (- n 1)))
)

;(trace countup)

;(countup '() 0 5)

; length_list without accumulator
(define (length_list ls)
        (if (null? ls)
        0
        (+ 1 (length_list (cdr ls)))))

;(trace length_list)
;(length_list '(1 2 3 4 5 6))

; length_list with accumulator
(define (length_list_acc length ls)
        (if (null? ls)
        length
        (length_list_acc (+ 1 length) (cdr ls)))
        )

;(length_list_acc 0 '(1 2 3 4 5 5))
(define (mdulo a b)
        (if ( < (- a b ) 0)
          a
          (modulo (- a b) b )))

;(modulo 17 5)
