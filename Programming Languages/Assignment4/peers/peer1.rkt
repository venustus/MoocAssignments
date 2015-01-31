#lang racket

;requires
(require rackunit)


;Dan's defs, for testing
(define ones (lambda () (cons 1 ones)))



;1  function sequence
;     input:  low, high, stride
;               all integers; stride positive
;     output: list of integers from low to high 
;              (x_n + stride = x_n+1); all x <= high
(define (sequence low high stride)
  (if(<= low high)
     (cons low (sequence (+ low stride) high stride))
     null))


;2  string-append-map
;     input:  xs      list
;             suffix  string
;     output: xs, with each element appended upon suffix
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))
  

;3  function list-nth-mod
;     input:  xs  list
;             n   number
;     output: ith element of xs
;               i = remainder(n / xs.length); first element of xs <==> (i = 0)th element
(define (list-nth-mod xs n)
  (car (list-tail xs (remainder n (length xs)))))
  ;(if(< n 0)
     ;"list-nth-mod: negative number"
     ;"list-nth-mod: empty list"
;  (check-true (< n 0) ["list-nth-mod: negative number"]))
  
  
  

;4  function stream-for-n-steps
;     input:  s  stream 
;             n  number, non-negative
;     output: list containing first n elements of s
(define (stream-for-n-steps s n)
  (define (get-next ss nn)
    (if (> nn 0)
        (cons 
         (car (ss)) (get-next (cdr (ss)) (- nn 1)))
        null))
  (get-next s n))
   
   
;5  function funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                (cons (* x -1) (lambda () (f (+ x 1))))
                (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))


;6  function dan-then-dog
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" (lambda () (dog))))]
           [dog (lambda () (cons "dog.jpg" (lambda () (dan))))])
    (lambda () (dan))))


;7  function stream-add-zero
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) 
                   (stream-add-zero (cdr (s)) ))))


;8  function cycle-lists

;9  function vector-assoc

;10 function cached-assoc



