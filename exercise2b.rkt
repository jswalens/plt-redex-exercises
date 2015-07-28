#lang racket

(require redex)

(require (except-in "common.rkt" lambda?))

(define-language TLambda
  (e ::=
     n
     +
     x
     (lambda ((x_!_ t) ...) e)
     (e e ...))
  (n ::= natural)
  (t ::=
     int
     (t ... -> t))
  (x ::= variable-not-otherwise-mentioned))
 
(define lambda? (redex-match? TLambda e))
 
(define e1
  (term (lambda ((x int) (f (int -> int))) (+ (f (f x)) (f x)))))
(define e2
  (term (lambda ((x int) (f ((int -> int) -> int))) (f x))))
(define e3
  (term (lambda ((x int) (x (int -> int))) x)))

(module+ test
  (test-equal (lambda? e1) #true)
  (test-equal (lambda? e2) #true)
  (test-equal (lambda? e3) #false))



; (⊢ Γ e t) – the usual type judgment for an LC language
 
(define-extended-language TLambda-tc TLambda
  (Γ ::= ((x t) ...))
  (v ::=
     n
     (lambda ((x t) ...) e))
  (E ::=
     hole
     (E e ...)
     (+ E e)
     (+ v E)
     ))
 
(module+ test
  (test-equal (judgment-holds (⊢ () ,e1 (int (int -> int) -> int))) #true)
  (test-equal (judgment-holds (⊢ () ,e2 t)) #false)
  (displayln  (judgment-holds (⊢ () ,e1 t) t))
  (displayln  (judgment-holds (⊢ () ,e2 t) t)))
 
(define-judgment-form TLambda-tc
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e t)
  [----------------------- "number"
   (⊢ Γ n int)]
 
  [----------------------- "+"
   (⊢ Γ + (int int -> int))]
 
  [----------------------- "variable"
   (⊢ Γ x (lookup Γ x))]
 
  [(⊢ (extend Γ (x_1 t_1) ...) e t)
   ------------------------------------------------- "lambda"
   (⊢ Γ (lambda ((x_1 t_1) ...) e) (t_1 ... -> t))]
 
  [(⊢ Γ e_1 (t_2 ... -> t))
   (⊢ Γ e_2 t_2) ...
   ------------------------------------------------- "application"
   (⊢ Γ (e_1 e_2 ...) t)])


; (extend Γ (x t) ...) add (x t) to Γ so that x is found before other x-s
(module+ test
  (test-equal (term (extend () (x int))) (term ((x int)))))
 
(define-metafunction TLambda-tc
  extend : Γ (x t) ... -> Γ
  [(extend ((x_Γ t_Γ) ...) (x t) ...) ((x t) ...(x_Γ t_Γ) ...)])
 
; (lookup Γ x) retrieves x's type from Γ
(module+ test
  (test-equal (term (lookup ((x int) (x (int -> int)) (y int)) x)) (term int))
  (test-equal (term (lookup ((x int) (x (int -> int)) (y int)) y)) (term int)))
 
(define-metafunction TLambda-tc
  lookup : Γ x -> t
  [(lookup ((x_1 t_1) ... (x t) (x_2 t_2) ...) x)
   t
   (side-condition (not (member (term x) (term (x_1 ...)))))]
  [(lookup any_1 any_2) ,(error 'lookup "not found: ~e" (term x))])


;; broken!!

(define ->
  (reduction-relation
   TLambda-tc
   #:domain e
   ;(--> e (lambda ((x int)) x))
   (--> (in-hole E (lambda ((x_1 t_1) ..._n) (e x_1 ..._n)))
        (in-hole E e) η)
   (--> (in-hole E ((lambda ((x_1 t_1) ..._n) e) e_1 ..._n))
        (in-hole E (subst ([e_1 x_1] ...) e)) β)
   ))

;;; end broken

(traces ->
      (term (((lambda ((x (int -> int))) x) (lambda ((x int)) x)) 1))
      #:pred (lambda (e) (judgment-holds (⊢ () ,e int))))


(module+ test
  (test-results))
