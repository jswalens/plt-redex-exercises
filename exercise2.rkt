#lang racket

(require redex)

(require "common.rkt")

(define-extended-language Lambda-calculus Lambda
  (e ::= .... n)
  (n ::= natural)
  (v ::= n (lambda (x ...) e))
 
  ; a context is an expression with one hole in lieu of a sub-expression 
  (E ::=
     hole
     (v ... E e ...)
     (lambda (x_!_ ...) E)))
 
(define Context? (redex-match? Lambda-calculus E))
 
(module+ test
  (define E1 (term ((lambda (x y) x) hole 1)))
  (define E2 (term ((lambda (x y) hole) 0 1)))
  (test-equal (Context? E1) #true)
  (test-equal (Context? E2) #true))

 
(define -->β
  (reduction-relation
   Lambda-calculus
   (--> (in-hole E ((lambda (x_1 ..._n) e) e_1 ..._n))
        (in-hole E (subst ([e_1 x_1] ...) e))
        β)))
 
(define example1 (term (lambda (x)
                         (((lambda (a b) a)
                           (lambda (y) 4) 7)
                          x))))

(define example2 (term (lambda (x) (a x))))

(define example3 (term (,example2 ,example2)))

(define example4 (term ((lambda (x) ((lambda (x) x) x)) (lambda (x) (a x)))))

(define example5 (term ((lambda (x) (a x)) b)))

(define -->βη
  (extend-reduction-relation
   -->β Lambda-calculus
   (--> (in-hole E (lambda (x_1 ..._n) (e x_1 ..._n)))
        (in-hole E e)
        η)))

(module+ test
  (test-->> -->βη #:equiv =α/racket example1 (term (lambda (y) 4)))
  (test-->> -->βη #:equiv =α/racket example2 (term a))
  (test-->> -->βη #:equiv =α/racket example3 (term (a a)))
  ; this test case fails for standard reduction, it gets stuck as 'a is not a value
  (test-->> -->βη #:equiv =α/racket example4 (term a))
  (test-->> -->βη #:equiv =α/racket example5 (term (a b)))
  
  ;(test-equal (term (eval-value ,example3)) (term (a a)))
  )


(module+ test
  (test-results))
