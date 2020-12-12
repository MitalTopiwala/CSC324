#lang racket #| * CSC324 Fall 2019: Assignment 1 * |#
#|
Module: dubdub
Description: Assignment 1: A More Featureful Interpreter
Copyright: (c)University of Toronto, University of Toronto Mississauga 
               CSC324 Principles of Programming Languages, Fall 2019

The assignment handout can be found at

    https://www.cs.toronto.edu/~lczhang/324/files/a1.pdf

Please see the assignment guidelines at 

    https://www.cs.toronto.edu/~lczhang/324/homework.html
|#

(provide run-interpreter)

(require "dubdub_errors.rkt")


;-----------------------------------------------------------------------------------------
; Main functions 
;-----------------------------------------------------------------------------------------
#|
(run-interpreter prog) -> any
  prog: datum?
    A syntactically-valid Dubdub program.

  Evaluates the Dubdub program and returns its value, or raises an error if the program is
  not semantically valid.
|#
(define (run-interpreter prog)
  ;check if its define or define-contract
 
  (cond
    
    [(not (list? (first prog))) (interpret (hash) (first prog))] ;base case ie. it is bool, number or ID
    [else
     (cond
      
      [(equal? (first(first prog)) 'define) (binding (reverse(rest(reverse prog))) (hash) (first(reverse prog)))] ;incorrect syntax?
      [(equal? (first(first prog)) 'define-contract)  (contract (third (first prog)) (second(first prog)))]
      [(builtin? (first(first prog)))  (interpret (hash) (first prog))]
      [(list? (first (first prog))) (if (> (length (first (first prog))) 1) (if (equal? (first(first(first prog))) 'lambda) (interpret (hash) (first prog)) (void)) (void) ) ]
      [else (report-error 'not-a-function (first prog))]
      )
    ]
   )
  
 )

(define (contract expr id)
  (cond
    [(builtin? (first expr)) (void)]
    [(equal? 'lambda (first (first expr))) (void)]
    [else (report-error 'invalid-contract id)]
   )
 )

(define (binding prog env expr) ;like make-hash
  (cond
    [(empty? prog) (interpret env expr)]
    [else
     (let ((first-define (first prog)))
       (if (hash-has-key? env (second first-define)) (report-error 'duplicate-name (second first-define)) 
       (binding (rest prog) (hash-set env (second first-define) (interpret env (third first-define))) expr)
       ))]
  )
)

(define (add-hashs args param fenv) ;add keys from param and corresponding value from args to fenv
  (foldl (lambda (key value hash_variable)
                                   (hash-set hash_variable key value))
                                   fenv
                                   param
                                   args)

)

;Website Used for split: https://stackoverflow.com/questions/25838354/splitting-list-with-racket

(define (my-take lst i)
  (if (> i 0)
      (cons (first lst)
            (my-take (rest lst) (- i 1)))
      '()))

(define (my-drop lst i)
  (if (> i 0)
      (my-drop (rest lst) (- i 1))
      lst))

(define (part lst i)
  (list (my-take lst i)
        (my-drop lst i)))

#|
(interpret env expr) -> any
  env: hash?
    The environment with which to evaluate the expression.
  expr: datum?
    A syntactically-valid Dubdub expression.

  Returns the value of the Dubdub expression under the given environment.
|#
(define (interpret env expr)
  
  (cond
    [(number? expr) expr]
    [(boolean? expr) expr]
    [(symbol? expr) (if (hash-has-key? env expr) (hash-ref env expr) (report-error 'unbound-name expr))]
    
    [else
     (let ((first-expr (first expr)))
     (cond
       [(equal? first-expr 'lambda) (list 'closure expr env)]
       
       ;builin functions check
       [(equal? first-expr '+) (apply + (map (lambda (add_lst) (interpret env add_lst)) (rest expr)))]
       [(equal? first-expr 'equal?) (apply equal? (map (lambda (add_lst) (interpret env add_lst)) (rest expr)))]
       [(equal? first-expr '<) (apply < (map (lambda (add_lst) (interpret env add_lst)) (rest expr)))]
       [(equal? first-expr 'integer?) (integer? (interpret env (second expr)))]
       [(equal? first-expr 'boolean?) (boolean? (interpret env (second expr)))]
       [(equal? first-expr 'procedure?) (or (builtin? (first (second expr))) (equal? 'lambda (first (second expr))))]
    
      
         ;Case handles lambda functions (and currying them)
         [else (let* ((closure_expr (if (hash-has-key? env (first expr)) (hash-ref env (first expr)) (interpret env (first expr))))
                 (args (map (lambda (lst) (interpret env lst)) (rest expr)))
                 (param (second (second closure_expr)))
                 (body (third (second closure_expr)))
                 (fenv (third closure_expr))) 

                 (cond
                   [(> (length args) (length param)) (report-error 'arity-mismatch (length args) (length param))]
                   [(equal? (length args) (length param))
                     (let* (
                     (final_env (foldl (lambda (key value hash_variable)
                                   (hash-set hash_variable key value))
                                   fenv
                                   param
                                   args))
                    
                      )
            
                       (interpret final_env body)
                   
     
                       )
                      ]
                   ;currying case
                   [else (let ((split (part param (length args))))
                         
                             ( list 'closure (list 'lambda (second split) body) (add-hashs args (first split) fenv)  )  )])   )]))
     ]))
             



;-----------------------------------------------------------------------------------------
; Helpers: Builtins and closures
;-----------------------------------------------------------------------------------------
; A hash mapping symbols for Dubdub builtin functions to their corresponding Racket value.
(define builtins
  (hash
   '+ +
   'equal? equal?
   '< <
   'integer? integer?
   'boolean? boolean?
   'procedure? procedure?
   ))

; Returns whether a given symbol refers to a builtin Dubdub function.
(define (builtin? identifier) (hash-has-key? builtins identifier))

#|
Starter definition for a closure "struct". Racket structs behave similarly to
C structs (contain fields but no methods or encapsulation).
Read more at https://docs.racket-lang.org/guide/define-struct.html.

You can and should modify this as necessary. If you're having trouble working with
Racket structs, feel free to switch this implementation to use a list/hash instead.
|#
(struct closure (params body))
