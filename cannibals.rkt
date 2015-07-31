#lang racket

(require redex)

(define-language Cannibals
  (person ::= missionary cannibal)
  (boat   ::=
          (person person)
          (person)
          empty)
  (river  ::= (boat water) (water boat))
  (state  ::= (person ... river person ...)))

(define startstate
  (term ((water empty) missionary missionary missionary cannibal cannibal cannibal)))

(module+ test
  (define (test-#t v)
    (test-equal v #true))

  (test-#t (redex-match? Cannibals state startstate)))

(define -->boat
  (reduction-relation
   Cannibals
   #:domain state
   (--> [person_1 ... (boat water) person_2 ...]
        [person_1 ... (water boat) person_2 ...]
        ;; TODO need side-condition that boat isn't empty to be able
        ;; to move.
        boat→right)
   (--> [person_1 ... (water boat) person_2 ...]
        [person_1 ... (boat water) person_2 ...]
        boat→left)
   ))


(module+ test

  (test--> -->boat startstate (term ((empty water)
                                     missionary missionary missionary
                                     cannibal cannibal cannibal)))
  )



(module+ test
  (test-results))
