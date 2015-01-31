
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
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([step (s)])
        (cons (car step) (stream-for-n-steps (cdr step) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= 0 (remainder x 5)) (- x) x) (lambda () (f (+ x 1)))))])
  (lambda () (f 1))))
    
(define dan-then-dog
  (letrec ([f (lambda (x) 
                (let ([dan-or-dog (if (string=? x "dog.jpg") "dan.jpg" "dog.jpg")]) 
                  (cons dan-or-dog (lambda () (f dan-or-dog)))))])
    (lambda () (f "dog.jpg"))))

(define (stream-add-zero s)
  (lambda () (let ([step (s)])
               (cons (cons 0 (car step)) (stream-add-zero (cdr step))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))  

(define (vector-assoc v vec)
  (letrec ([vlen (vector-length vec)]
           [f (lambda (i)
                (if (>= i vlen)
                    #f                    
                    (let ([vi (vector-ref vec i)])
                      (if (and (pair? vi) (equal? v (car vi)))
                          vi
                          (f (+ i 1))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([vec (make-vector n #f)]       
           [pos 0]
           [cached-result (lambda (result) 
                            (and (> n 0)
                                 (begin
                                   (vector-set! vec pos result)
                                   (set! pos (if (= pos (- n 1)) 0 (+ pos 1)))))
                            result)])
    (lambda (v) (let ([cv (vector-assoc v vec)])
                  (if cv
                      cv
                      (let ([result (assoc v xs)])
                        (if result
                            (cached-result result)
                            #f)))))))
  
                          
               