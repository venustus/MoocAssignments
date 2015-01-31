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

(define (racketlist->mupllist list)
  (if (null? list)
      (aunit)
      (apair (car list) (racketlist->mupllist (cdr list)))))

(define (mupllist->racketlist list)
  (if (aunit? list)
      null
      (cons (apair-e1 list) (mupllist->racketlist (apair-e2 list)))))

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
        [(int? e)
         e]
        [(aunit? e)
         e]
        [(closure? e)
         e]
        [(fun? e)
         (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([binding (cons (mlet-var e) (eval-under-env (mlet-e e) env))])
           (eval-under-env (mlet-body e) (cons binding env)))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? c)
               (let ([f (closure-fun c)])
                 (let ([var-env (cons (cons (fun-formal f) arg) (closure-env c))]
                       [name (fun-nameopt f)])
                   (let ([name-var-env (if name (cons (cons name c) var-env) var-env)])
                     (eval-under-env (fun-body f) name-var-env))))
               (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([r (eval-under-env (isaunit-e e) env)])
           (if (aunit? r)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([binding (car lstlst)])
        (mlet (car binding) (cdr binding) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "mapper"
       (fun "functor" "list"
            (ifaunit (var "list")
                     (aunit)
                     (apair (call (var "mapper") (fst (var "list")))
                            (call (var "functor") (snd (var "list"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "n"
             (call (var "map")
                   (fun #f "v"
                        (add (var "v") (var "n")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(require racket/set)

(define (compute-free-vars e)
  (letrec ([aux (lambda (exp)
                  (cond [(var? exp)
                         (set (var-string exp))]
                        [(add? exp)
                         (let ([s1 (aux (add-e1 exp))]
                               [s2 (aux (add-e2 exp))])
                           (set-union s1 s2))]
                        [(ifgreater? exp)
                         (let ([s1 (aux (ifgreater-e1 exp))]
                               [s2 (aux (ifgreater-e2 exp))]
                               [s3 (aux (ifgreater-e3 exp))]
                               [s4 (aux (ifgreater-e4 exp))])
                           (set-union s1 s2 s3 s4))]
                        [(call? exp)
                         (let ([s1 (aux (call-funexp exp))]
                               [s2 (aux (call-actual exp))])
                           (set-union s1 s2))]
                        [(mlet? exp)
                         (let ([es (aux (mlet-e exp))]
                               [bodys (aux (mlet-body exp))])
                           (set-union es (set-remove bodys (mlet-var exp))))]
                        [(apair? exp)
                         (let ([s1 (aux (apair-e1 exp))]
                               [s2 (aux (apair-e2 exp))])
                           (set-union s1 s2))]
                        [(fst? exp)
                         (aux (fst-e exp))]
                        [(snd? exp)
                         (aux (snd-e exp))]
                        [(isaunit? exp)
                         (aux (isaunit-e exp))]
                        [(fun? exp)
                         (let ([name (fun-nameopt exp)]
                               [formal (fun-formal exp)]
                               [free (aux (fun-body exp))])
                           (set-remove (set-remove free formal) name))]
                        [#t
                         (set)]))])
    (cond [(fun? e)
           (let ([free (set-remove (set-remove (aux (fun-body e)) (fun-formal e)) (fun-nameopt e))])
             (fun-challenge (fun-nameopt e) (fun-formal e) (compute-free-vars (fun-body e)) free))]
          [(add? e)
           (let ([e1 (compute-free-vars (add-e1 e))]
                 [e2 (compute-free-vars (add-e2 e))])
             (add e1 e2))]
          [(ifgreater? e)
           (let ([e1 (compute-free-vars (ifgreater-e1 e))]
                 [e2 (compute-free-vars (ifgreater-e2 e))]
                 [e3 (compute-free-vars (ifgreater-e3 e))]
                 [e4 (compute-free-vars (ifgreater-e4 e))])
             (ifgreater e1 e2 e3 e4))]
          [(call? e)
           (let ([e1 (compute-free-vars (call-funexp e))]
                 [e2 (compute-free-vars (call-actual e))])
             (call e1 e2))]
          [(mlet? e)
           (let ([exp (compute-free-vars (mlet-e e))]
                 [body (compute-free-vars (mlet-body e))])
             (mlet (mlet-var e) exp body))]
          [(apair? e)
           (let ([e1 (compute-free-vars (apair-e1 e))]
                 [e2 (compute-free-vars (apair-e2 e))])
             (apair e1 e2))]
          [(fst? e)
           (fst (compute-free-vars (fst-e e)))]
          [(snd? e)
           (snd (compute-free-vars (snd-e e)))]
          [(isaunit? e)
           (isaunit (compute-free-vars (isaunit-e e)))]
          [#t
           e])))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e)
         e]
        [(aunit? e)
         e]
        [(closure? e)
         e]
        [(fun-challenge? e)
         (let ([f (fun (fun-challenge-nameopt e) (fun-challenge-formal e) (fun-challenge-body e))]
               [free (fun-challenge-freevars e)])
           (closure (filter (lambda (binding) (set-member? free (car binding))) env) f))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([binding (cons (mlet-var e) (eval-under-env-c (mlet-e e) env))])
           (eval-under-env-c (mlet-body e) (cons binding env)))]
        [(call? e)
         (let ([c (eval-under-env-c (call-funexp e) env)]
               [arg (eval-under-env-c (call-actual e) env)])
           (if (closure? c)
               (let ([f (closure-fun c)])
                 (let ([var-env (cons (cons (fun-formal f) arg) (closure-env c))]
                       [name (fun-nameopt f)])
                   (let ([name-var-env (if name (cons (cons name c) var-env) var-env)])
                     (eval-under-env-c (fun-body f) name-var-env))))
               (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([p (eval-under-env-c (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([p (eval-under-env-c (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([r (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? r)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
