#lang racket

(require redex)

(require "extend-lookup.rkt" "close.rkt" "common.rkt")

(define-extended-language Assignments Lambda
  (e ::= .... n + (void) (set! x e))
  (n ::= natural))

; (let ((x_1 x_2) ...) e_1 e_2) binds the current value of x_2 to x_1,
; evaluates e_1, throws away its value, and finally evaluates e_2 
(define-metafunction Assignments
  let : ((x e) ...) e e -> e
  [(let ([x_lhs e_rhs] ...) e_1 e_2)
   ((lambda (x_lhs ...)
      ((lambda (x_dummy) e_2) e_1))
    e_rhs ...)
   (where (x_dummy) ,(variables-not-in (term (e_1 e_2)) '(dummy)))])

(define e1
  (term
   (lambda (x)
     (lambda (y)
       (let ([tmp x])
         (set! x (+ y 1))
         tmp)))))
 
(define p-1 (term ((,e1 1) 2)))
 
(define e2
  (term
   ((lambda (x)
      (let ([tmp x])
        (set! x y)
        tmp))
    (let ([tmp-z z])
      (set! z (+ z 1))
      (let ([tmp-y y])
        (set! y tmp-z)
        tmp-y)))))
 
(define p-2
  (term ((lambda (y) ((lambda (z) ,e2) 1)) 2)))

(define-extended-language Assignments-cesk Assignments
  (ρ ::= ((x l) ...)) ; environment
  (c ::= (v ρ)) ; closure
  (k ::= (frame ...))  ; kontinuation
  (frame ::=
         (app [c ...] ρ [e ...])
         (setf l))
  (σ ::= ((l c) ...)) ; store
  (l ::= natural)
  (v ::= n + (void) (lambda (x ...) e)))
 

(define s->βs
  (reduction-relation
   Assignments-cesk
   #:domain (e ρ σ k)
   (--> [x σ]
        [(in-hole E (lookup σ x)) σ]
        CESK-lookup)
   (--> [(in-hole E (set! x v)) σ]
        [(in-hole E (void)) (extend σ (x) (v))])
   (--> [(in-hole E (+ n_1 n_2)) σ]
        [(in-hole E ,(+ (term n_1) (term n_2))) σ])
   (--> [(in-hole E ((lambda (x ..._n) e) v ..._n)) σ]
        [(in-hole E (subst ((x_new x) ...) e)) (extend σ (x_new ...) (v ...))]
        (where (x_new ...) ,(variables-not-in (term σ) (term (x ...)))))))







(module+ test
  (test-results))
