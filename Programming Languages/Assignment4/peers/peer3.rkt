#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond
   [(< n 0) (error "list-nth-mod: negative number")]
   [(empty? xs) (error "list-nth-mod: empty list")]
   [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pair (s)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

(define (next n)
  (lambda () (cons (if (= 0 (remainder n 5)) (- n) n) (next (+ n 1)))))

(define funny-number-stream (next 1))

(define dan-then-dog (lambda () (cons "dan.jpg" dog-then-dan)))
(define dog-then-dan (lambda () (cons "dog.jpg" dan-then-dog)))

(define (stream-add-zero s)
  (lambda ()
    (let ([pair (s)])
      (cons (cons 0 (car pair)) (stream-add-zero (cdr pair))))))

(define (cycle-lists xs ys)
  (define (next n)
    (lambda ()
      (cons (cons (list-nth-mod xs n)
                  (list-nth-mod ys n)) (next (+ n 1)))))
  (next 0))

(define (vector-assoc v vec)
  (if (= 0 (vector-length vec))
      #f
      (let ([x (vector-ref vec 0)])
        (if (and (pair? x) (equal? (car x) v))
            x
            (vector-assoc v (vector-drop vec 1))))))

(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define index 0)
  (lambda (v)
    (or (vector-assoc v cache)
        (let ([result (assoc v xs)])
          (and result
               (begin
                 (vector-set! cache index result)
                 (set! index (remainder (add1 index) n))
                 result))))))
