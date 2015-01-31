#lang racket
;; Programming Languages Homework5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw5.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (int 3) (var "a") (add 3 4))) (apair (int 3) (apair (var "a") (apair (add 3 4) (aunit)))) "including variables and add operations")
   (check-equal? (racketlist->mupllist null) (aunit) "racketlist->mupllist test")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (var "a") (apair (add 3 4) (aunit))))) (list (int 3) (var "a") (add 3 4)) "including variables and add operations")
   (check-equal? (mupllist->racketlist (aunit)) null "empty list test")
   
   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-exp (int 3)) (int 3) "int should evaluate to itself")
   (check-equal? (eval-exp (aunit)) (aunit) "unit should evaluate to itself")
   (check-equal? (eval-exp (apair (int 3) (apair (int 4) (aunit)))) (apair (int 3) (apair (int 4) (aunit))) "pair of values should evaluate to itself")
   (check-equal? (eval-exp (fun "test" "x" (add (var "x") (int 7)))) (closure null (fun "test" "x" (add (var "x") (int 7)))) "simple function should evaluate to closure holding current environment")
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   (check-equal? (eval-exp (mlet "x" (int 1) (mlet "y" (int 2) (apair (var "x") (apair (var "y") (aunit)))))) (apair (int 1) (apair (int 2) (aunit))) "mlet test 2")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (fun #f "x" (add (var "x") (int 7))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (mlet "y" (int 2) (mlet "myfunc" (fun #f "x" (add (var "x") (var "y"))) 
                                                   (add (call (var "myfunc") (int 1))
                                                        (call (var "myfunc") (int 10)))))) (int 15) "call with mlet test")
   
   ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "fst test")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 20))) (mlet "myfunc" (fun "#f" "z" (add (var "z") (var "y"))) 
                                                                                       (apair (call (var "myfunc") (int 1))
                                                                                            (call (var "myfunc") (var "x")))))) (apair (int 21) (int 30)))
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "ifeq test 2")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (ifgreater (var "x") (int 7) (int 1) (int 0)))) (apair (int 1) (apair (int 8) (apair (int 3) (apair (int 10) (aunit))))))) 
                 (apair (int 0) (apair (int 1) (apair (int 0) (apair (int 1) (aunit))))) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
