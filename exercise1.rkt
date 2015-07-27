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

(module+ test
  (test-results))
