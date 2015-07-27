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
  bv : e -> any
  [(bv x) ()]
  [(bv (lambda (x ...) e)) ,(append (term (x ...)) (term (bv e)))]
  [(bv (e ...)) ,(apply append (map (lambda (x) (term (bv ,x))) (term (e ...))))])


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
  lookup : x env -> any
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

(define-metafunction Env
  let : env e -> e
  [(let ((x e) ...) e_1) ((lambda (x ...) e_1) e ...)])

(module+ test
  (test-results))
