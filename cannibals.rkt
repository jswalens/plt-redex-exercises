#lang racket

(require redex)

(define-language Cannibals
  (person ::= missionary cannibal)
  (boat   ::=
          (person person)
          (person)
          ())
  (river  ::= (boat water) (water boat))
  (state  ::= ((person ...) river (person ...))))

(define startstate
  (term (() (water ()) (missionary missionary missionary cannibal cannibal cannibal))))

(module+ test
  (define (test-#t v)
    (test-equal v #true))

  (test-#t (redex-match? Cannibals state startstate)))

(define -->boat
  (reduction-relation
   Cannibals
   #:domain state
   (--> [(person_1 ...) (boat water) (person_2 ...)]
        [(person_1 ...) (water boat) (person_2 ...)]
        (side-condition (not (eq? (term boat) (term ())))) ;; boat not empty!
        boat→)
   (--> [(person_1 ...) (water boat) (person_2 ...)]
        [(person_1 ...) (boat water) (person_2 ...)]
        (side-condition (not (eq? (term boat) (term ())))) ;; boat not empty!
        boat←)
   (--> [(person_1 ...) (water (         person_b ...)) (person_2 ... person_i person_n ...)]
        [(person_1 ...) (water (person_i person_b ...)) (person_2 ...          person_n ...)]
        (side-condition (< (length (term (person_b ...))) 2))
        (side-condition (>= (missionaries (term (person_2 ... person_n ...)))
                            (cannibals    (term (person_2 ... person_n ...)))))
        embark-right)
   (--> [(person_1 ... person_i person_n ...)  ((         person_b ...) water) (person_2 ...)]
        [(person_1 ...          person_n ...)  ((person_i person_b ...) water) (person_2 ...)]
        (side-condition (< (length (term (person_b ...))) 2))
        (side-condition (>= (missionaries (term (person_1 ... person_n ...)))
                            (cannibals    (term (person_1 ... person_n ...)))))
        embark-left)
   (--> [(person_1 ...) (water (person_a ... person_i person_b ...)) (                person_n ...) ]
        [(person_1 ...) (water (person_a ...          person_b ...)) (order (person_i person_n ...))]
        (side-condition (>= (missionaries (term (person_i person_n ...)))
                            (cannibals    (term (person_i person_n ...)))))
        disembark-right)
   (--> [(       person_2 ...          ) ((person_a ... person_i person_b ...) water) (person_1 ...)]
        [(order (person_2 ... person_i)) ((person_a ...          person_b ...) water) (person_1 ...)]
        (side-condition (>= (missionaries (term (person_i person_2 ...)))
                            (cannibals    (term (person_i person_2 ...)))))
        disembark-left)
   ))

(define-metafunction Cannibals
  order : (person ...) -> (person ...)
  [(order (person ...))
   ,(sort (term (person ...)) (lambda (x y) (string<=? (symbol->string x)
                                                       (symbol->string y))))])

(define (cannibals l)
  (length (remove* '(missionary) l)))
(define (missionaries l)
  (length (remove* '(cannibal) l)))

(module+ test

  ;; we manage to embark one cannibal from right.
  (test-->>∃ -->boat startstate (term (() (water (cannibal))
                                       (cannibal cannibal
                                                 missionary missionary missionary))))

  ;; we manage to transfer everybody.
  (test-->>∃ -->boat startstate (term ((cannibal cannibal cannibal
                                                 missionary missionary missionary
                                                 (() water) ()))))
  )



(module+ test
  (stepper -->boat startstate)
  (test-results))
