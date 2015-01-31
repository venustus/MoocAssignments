;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

; 1a - converts a racket list to a mupl list
; uses apair constructor to create new elements of mupl list 
; assumes that elements of input list l are valid mupl values
(define (racketlist->mupllist l)
  (if (null? l) (aunit) 
      (apair (car l) (racketlist->mupllist (cdr l)))))

; 1b = converts a mupl list to a racket list
; extracts elements from mupl list using apair-e1 and apair-e2 
; raises error if ml is not a valid apair
(define (mupllist->racketlist ml)
  (cond [(aunit? ml) null]
        [(apair? ml) (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))]
        [#t (error "not a mupl list")]))
      

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(apair? e) 
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e) 
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([arg (eval-under-env (call-actual e) env)] ; evaluate the argument and store in 'arg'
               [funval (eval-under-env (call-funexp e) env)]) ; evaluate the function expression to a closure and store in 'funval'
           (if (not (closure? funval))
               (error "called expression is not a function")
               (let ([fundef (closure-fun funval)]) ; extract function definition from closure
                 (eval-under-env
                  (fun-body fundef) ; evaluate the function body under the following env
                  (let ([extended-env-1 (cons (cons (fun-formal fundef) 
                                                    (eval-under-env arg env)) (closure-env funval))]) ; argument should definitely be added
                    (if (not (fun-nameopt fundef)) ; if function does not have a name
                        extended-env-1 ; don't add anything else
                        (cons (cons (fun-nameopt fundef) funval) extended-env-1)) ; otherwise add the function name itself to the env
                      )))))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (not (apair? v))
               (error "cannot get fst of non-pair")
               (apair-e1 v)))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (not (apair? v))
               (error "cannot get snd of non-pair")
               (apair-e2 v)))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

; 3a
(define (ifaunit e1 e2 e3) 
  (ifgreater (isaunit e1) (int 0) e2 e3))

; 3b
(define (mlet* lstlst e2) 
  (if (null? lstlst)
      e2
      (let ([binding (car lstlst)])
        (mlet (car binding) (cdr binding) (mlet* (cdr lstlst) e2))))) 

; 3c
; x = y if and only if both x > y and x < y are false
(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4
; 4a - the function map must be named as it must be called recursively
(define mupl-map 
  (fun #f "mapper" 
       (fun "map" "xs" 
            (ifaunit (var "xs") 
                     (aunit)
                     (apair (call (var "mapper") (fst (var "xs")))
                            (call (var "map") (snd (var "xs"))))))))

; 4b
; just use the previous map function
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" 
             (fun #f "xs" 
                  (call (call (var "map") 
                              (fun #f "j" (add (var "i") (var "j"))))
                        (var "xs"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
