#lang racket

(require redex)
(require "common.rkt")

;; -----------------------------------------------------------------------------
;; (bv e) determines the bound variables in e.

(module+ test
  (test-equal (term (bv x)) '())
  (test-equal (term (bv (lambda (x) x))) '(x))
  (test-equal (term (bv (lambda (x y) (x y)))) '(x y))
  (test-equal (term (bv ((lambda (a) a) (lambda (b c) (lambda (d) e))))) '(a b c d)))

(define-metafunction Lambda
  bv : e -> (x ...)
  [(bv x) ()]
  [(bv (lambda (x ...) e)) ,(append (term (x ...)) (term (bv e)))]
  [(bv (e ...)) (x ... ...)
                (where ((x ...) ...) ((bv e) ...))])


(define-extended-language Env Lambda
  (e ::= .... natural)
  (env ::= ((x e) ...)))

(define env1 (term ((x 1)
                    (y (lambda (x) x)))))
(define env2 (term ((x 0) (x 1)
                    (x (y z))
                    (y (lambda (x) x)))))

(module+ test
  (test-equal (redex-match? Env env env1) #t)
  (test-equal (redex-match? Env env (term ())) #t)
  (test-equal (redex-match? Env env env2) #t))

(module+ test
  (test-equal (term (lookup x ,env1)) 1)
  (test-equal (term (lookup x ,env2)) 0))


(define-metafunction Env
  lookup : x env -> e or #false
  [(lookup _ ()) #false]
  [(lookup x_1 ((x_1 any_1) (x_2 any_2) ...)) any_1]
  [(lookup x_1 ((x_0 any_0) (x_2 any_2) ...)) (lookup x_1 ((x_2 any_2) ...))])

(module+ test
  (test-equal (term
               (let ((x (lambda (a b c) a))
                     (y (lambda (x) x)))
                 (x y y y)))
              (term
               ((lambda (x y) (x y y y))
                (lambda (a b c) a)
                (lambda (x) x)))))

;; let is a syntactic sugar construct allowing local bindings.
(define-metafunction Env
  let : ((x any) ...) any -> any
  [(let ((x_lhs any_rhs) ...) any_body)
   ((lambda (x_lhs ...) any_body) any_rhs ...)])

(module+ test
  (test-equal (term (fv x)) '(x))
  (test-equal (term (fv (lambda (x) x))) '())
  (test-equal (term (fv ((lambda (x) x) y))) '(y))
  (test-equal (term (fv (z y))) '(z y)))

(define-metafunction Env
  fv : e -> (x ...)
  [(fv x) (x)]
  [(fv (lambda (x ...) e)) ,(remove* (term (x ...)) (term (fv e)))  ]
  [(fv (e ...)) (x ... ...)
                (where ((x ...) ...) ((fv e) ...))])


(module+ test
  (test-results))
