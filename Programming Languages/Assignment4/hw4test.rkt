#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4.rkt")
(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define ca (cached-assoc (list (cons 1 2) (cons 3 4) (cons 4 5) (cons 5 6) (cons 6 7)) 3))

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")
   (check-equal? (sequence 3 11 2) (list 3 5 7 9 11) "Sequence test 2")
   (check-equal? (sequence 3 8 3) (list 3 6) "Sequence test 3")
   (check-equal? (sequence 3 2 1) null "Empty results test")
   
   ; string-append-map test
   (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")
   (check-equal? (string-append-map null ".jpg") null)
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")
   (check-equal? (list-nth-mod (list 6 8 2 3 4) 6) 8 "list-nth-mod test 2")
   (check-equal? (list-nth-mod (list 6 8 2 3 4) 10) 6 "list-nth-mod test 3")
   
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps (lambda () (cons 1 ones)) 1) (list 1) "stream-for-n-steps test")
   (check-equal? (stream-for-n-steps powers-of-two 5) (list 2 4 8 16 32))
   (check-equal? (stream-for-n-steps powers-of-two 0) null)
   (check-equal? (stream-for-n-steps powers-of-two 1) (list 2))
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")
   (check-equal? (stream-for-n-steps funny-number-stream 0) null)
   (check-equal? (stream-for-n-steps funny-number-stream 1) (list 1))
   
   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test")
   (check-equal? (stream-for-n-steps dan-then-dog 2) (list "dan.jpg" "dog.jpg") "dan-then-dog test 2")
   (check-equal? (stream-for-n-steps dan-then-dog 5) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg"))
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test")
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b" "c")) 4) (list (cons 1 "a") (cons 2 "b") (cons 3 "c") (cons 1 "a")))
   (check-equal? (stream-for-n-steps (cycle-lists (list 1) (list "a")) 2) (list (cons 1 "a") (cons 1 "a")))
                 
   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")
   (check-equal? (vector-assoc 6 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) #f)
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) #t (cons 4 1) (cons 5 1) (list 1 2 3))) (cons 4 1))
   
   ; cached-assoc tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
   (check-equal? (ca 7) #f)
   (check-equal? (ca 1) (cons 1 2))
   (check-equal? (ca 4) (cons 4 5))
   (check-equal? (ca 5) (cons 5 6))
   (check-equal? (ca 4) (cons 4 5))
   (check-equal? (ca 3) (cons 3 4))
   
;   ; while-less test
;   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
