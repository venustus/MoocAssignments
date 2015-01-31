
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; problem-1 simple recursive function for counting in a range with a given stride
(define (sequence low high stride)
  (if (> low high) 
      null 
      (cons low (sequence (+ low stride) high stride))))

; problem-2 just using map and string-append
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; problem-3 using cond construct and remainder, list-tail functions
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; problem-4 counts i from n to 0 and cons the car of the stream each time
(define (stream-for-n-steps s n)
  (letrec ([helper (lambda (stream i)
                  (if (= i 0) null
                      (let ([pr (stream)])
                        (cons (car pr) 
                               (helper (cdr pr) (- i 1))))))])
    (helper s n)))

; problem-5 just a regular recursive function with an if condition for multiples of 5
(define funny-number-stream
  (letrec ([f (lambda (x) 
                (cons (if (= (remainder x 5) 0) (- 0 x) x) 
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; problem-6 the if condition toggles between dan anddog
(define dan-then-dog
  (letrec ([f (lambda (x)
                (let ([str (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")])
                  (cons str (lambda () (f str)))))])
    (lambda () (f "dog.jpg"))))

; problem-7 just like a function using a stream, but also create a stream at the same time
(define (stream-add-zero s)
  (letrec ([helper (lambda (stream)
                     (let ([pr (stream)]) 
                       (cons (cons 0 (car pr)) (lambda () (helper (cdr pr))))))])
    (lambda () (helper s))))

; problem-8 use list-nth-mod function from before
(define (cycle-lists xs ys)
  (letrec ([helper (lambda (n)
                     (cons 
                      (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                      (lambda () (helper (+ n 1)))))])
  (lambda () (helper 0))))

; problem-9 use cond construct and pair? to check for pair
; check one by one from 0 through n - 1
(define (vector-assoc v vec)
  (letrec ([helper (lambda (i)
                    (cond [(> i (- (vector-length vec) 1)) #f] 
                          [(pair? (vector-ref vec i)) 
                           (let ([elem (vector-ref vec i)])
                             (if (equal? v (car elem)) elem (helper (+ i 1))))]
                          [#t (helper (+ i 1))]))])
  (helper 0)))
  
; problem-10 
; cache is the vector holding the cache elements
; next-slot is the next slot to be filled in case of a cache miss
; next-slot is computed easily using a remainder function
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [next-slot 0])
    (lambda (v)
      (let ([ans (vector-assoc v cache)])
        (if ans ans 
            (let ([ans (assoc v xs)]) 
              (if ans (begin (vector-set! cache next-slot ans) 
                             (set! next-slot (remainder (+ next-slot 1) n))
                             ans) #f))))))) 