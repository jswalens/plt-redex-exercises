#lang racket

(require redex)

(require "common.rkt")

(define-extended-language Lambda-calculus Lambda
  (e ::= .... n)
  (n ::= natural)
  (v ::= n (lambda (x ...) e))
 
  ; a context is an expression with one hole in lieu of a sub-expression 
  (C ::=
     hole
     (e ... C e ...)
     (lambda (x_!_ ...) C)))
 
(define Context? (redex-match? Lambda-calculus C))
 
(module+ test
  (define C1 (term ((lambda (x y) x) hole 1)))
  (define C2 (term ((lambda (x y) hole) 0 1)))
  (test-equal (Context? C1) #true)
  (test-equal (Context? C2) #true))

 
(define -->β
  (reduction-relation
   Lambda-calculus
   (--> (in-hole C ((lambda (x_1 ..._n) e) e_1 ..._n))
        (in-hole C (subst ([e_1 x_1] ...) e))
        β)))
 

(module+ test
  (test-results))
