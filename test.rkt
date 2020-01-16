#lang racket

(require
  quickcheck rackunit/quickcheck
  rackunit rackunit/text-ui
  "./main.rkt"
  )

(define (test-boolean)
  (test-case
    "test-boolean"
    (check-property
      (property ([bool arbitrary-boolean])
        (eqv? (ltl-eval bool) bool)
        ))
    )
  )

(define (test-not)
  (test-case
    "test-not"
    (check-property
      (property ([bool arbitrary-boolean])
        (eqv? (ltl-eval (ltl-formula 'not bool)) (not bool))
        ))
    )
  )

(define (test-and)
  (test-case
    "test-and"
    (check-property
      (property ([lst (arbitrary-list arbitrary-boolean)])
        (eqv? (ltl-eval (ltl-formula 'and lst)) (not (index-of lst #f)))
        ))
    )
  )

(define (test-or)
  (test-case
    "test-or"
    (check-property
      (property ([lst (arbitrary-list arbitrary-boolean)])
        (eqv? (ltl-eval (ltl-formula 'or lst)) (not (not (index-of lst #t))))
        ))
    )
  )

(define (test-next)
  (test-case
    "test-next"
    (check-property
      (property ([bool arbitrary-boolean])
        (eqv? (ltl-eval (ltl-eval (ltl-formula 'next bool))) bool)
        ))
    )
  )

(define (test-always)
  (test-case
    "test-always"
    (check-property
      (property ([lst (arbitrary-list arbitrary-boolean)])
        (define expected (andmap identity lst))
        (define formula (ltl-formula 'always 'a))
        (define f
          (foldl
            (lambda (v l) (ltl-eval l (lambda (dummy) v)))
            formula
            lst))
        (define actual (if (not (boolean? f)) #t f))
        (eqv? actual expected)
        ))
    )
  )

(define (test-eventually)
  (test-case
    "test-eventually"
    (check-property
      (property ([lst (arbitrary-list arbitrary-boolean)])
        (define expected (ormap identity lst))
        (define formula (ltl-formula 'eventually 'a))
        (define f
          (foldl
            (lambda (v l) (ltl-eval l (lambda (_) v)))
            formula
            lst))
        (define actual (if (boolean? f) f #f))
        (eqv? actual expected)
        ))
    )
  )

(module+ test
  (define/provide-test-suite ltl-tests
    (test-boolean)
    (test-not)
    (test-and)
    (test-or)
    (test-next)
    (test-always)
    (test-eventually)
    )
  (run-tests ltl-tests)
  )
