#lang racket

(require redex "common.rkt" "extend-lookup.rkt")

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

(define-extended-language Assignments-s Assignments
  (E ::= hole (v ... E e ...) (set! x E))
  (σ ::= ((x v) ...))
  (v ::= n + (void) (lambda (x ...) e)))
 

(define s->βs
  (reduction-relation
   Assignments-s
   #:domain (e σ)
   (--> [(in-hole E x) σ]
        [(in-hole E (lookup σ x)) σ])
   (--> [(in-hole E (set! x v)) σ]
        [(in-hole E (void)) (extend σ (x) (v))])
   (--> [(in-hole E (+ n_1 n_2)) σ]
        [(in-hole E ,(+ (term n_1) (term n_2))) σ])
   (--> [(in-hole E ((lambda (x ..._n) e) v ..._n)) σ]
        [(in-hole E (subst ((x_new x) ...) e)) (extend σ (x_new ...) (v ...))]
        (where (x_new ...) ,(variables-not-in (term σ) (term (x ...)))))))

(module+ test
  (define bugfix (term ((let ((x 4))
                           (void)
                           (let ((x 8))
                             (void)
                             x)) ())))
  (test-->>∃ s->βs bugfix (redex-match? Assignments-s [8 σ]))
  ;(traces s->βs bugfix)

  ;; TODO testme
  (define duplicates (term (letrec ((x 5)
                                    (x 6))
                             x)))

  )

(define-extended-language ImperativeCalculus Assignments-s
  (e ::= .... (letrec ((x v) ...) e) (begin e ...))
  (E ::= .... (begin v ... E e ...)))

(module+ test
  (define example1 (term ((letrec ((x (lambda (iets) (y iets)))
                                   (y (lambda (z)    (+ z 41))))
                            (x (y 4))) ())))

  (define example2 (term (let ((x 4)
                               (y 5))
                           (void)
                           (+ x y))))

  (define example3 (term (letrec ((x 4)
                                  (y 5))
                           (+ x y))))

  (test-equal (redex-match? ImperativeCalculus (e σ) example1) #t)
  (test-equal (redex-match? ImperativeCalculus e example2) #t)
  (test-equal (redex-match? ImperativeCalculus e example3) #t))

(define letrec-s->βs
  (extend-reduction-relation
   s->βs
   ImperativeCalculus
   (--> [(in-hole E (letrec ((x v) ...) e)) σ]
        [(in-hole E e)
         (extend σ (x ...) (v ...))]
        letrec)
   (--> [(in-hole E (begin v ... v_1)) σ]
        [(in-hole E v_1) σ]
        begin)))


(module+ test
  ;(redex-match ImperativeCalculus [e σ] example1)
  (test-->>∃ letrec-s->βs example1 (redex-match? ImperativeCalculus [86 σ])))


(module+ test
  (define example4 (term
                    ((letrec ((x (lambda () y))
                              (y 1))
                       (x))
                     ())))
  (define example5 (term
                    ((letrec ((x (lambda () y))
                              (y 1))
                       (begin
                         (set! y 2)
                         (x)))
                     ())))
  (test-equal (redex-match? ImperativeCalculus (e σ) example4) #t)
  (test-->>∃ letrec-s->βs example4 (redex-match? ImperativeCalculus [1 σ]))
  (test-equal (redex-match? ImperativeCalculus (e σ) example5) #t)
  (test-->>∃ letrec-s->βs example5 (redex-match? ImperativeCalculus [2 σ])))

(module+ test
  (test-results))
