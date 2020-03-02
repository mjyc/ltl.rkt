#lang racket

(require
  quickcheck rackunit/quickcheck
  rackunit rackunit/text-ui
  "./main2.rkt"
  )

(define (test-val)
  (test-case
    "test-val"
    (check-property
      (property ([bool arbitrary-boolean])
        (eqv? (ltlinterpret (ltlval bool)) bool)
        )
      )
    )
  )

(define (test-not)
  (test-case
    "test-not"
    (check-property
      (property ([bool arbitrary-boolean])
        (eqv? (ltlinterpret (ltlnot (ltlval bool))) (not bool))
        )
      )
    )
  )

(define (test-and)
  (test-case
    "test-and"
    (check-property
      (property ([bool1 arbitrary-boolean] [bool2 arbitrary-boolean])
        (eqv?
          (ltlinterpret (ltland (ltlval bool1) (ltlval bool2)))
          (andmap identity (list bool1 bool2))
          )
        )
      )
    )
  )

(define (test-or)
  (test-case
    "test-or"
    (check-property
      (property ([bool1 arbitrary-boolean] [bool2 arbitrary-boolean])
        (eqv?
          (ltlinterpret (ltlor (ltlval bool1) (ltlval bool2)))
          (ormap identity (list bool1 bool2))
          )
        )
      )
    )
  )

(define (test-if)
  (test-case
    "test-if"
    (check-property
      (property ([bool1 arbitrary-boolean] [bool2 arbitrary-boolean])
        (eqv?
          (ltlinterpret (ltlif (ltlval bool1) (ltlval bool2)))
          (or (not bool1) bool2)
          )
        )
      )
    )
  )

(define (test-next)
  (test-case
    "test-next"
    (check-property
      (property ([bool arbitrary-boolean])
        (eqv? (ltlinterpret (ltlinterpret (ltlnext (ltlval bool)))) bool)
        )
      )
    )
  )

(define (test-always)
  (test-case
    "test-always"
    (check-property
      (property ([lst (arbitrary-list arbitrary-boolean)])
        (define expected (andmap identity lst))
        (define formula (ltlalways (ltlval 'a)))
        (define stream (map (lambda (x) (if x 'a x)) lst))
        (define actual
          (ltleval formula stream))
        (displayln (list "actual expected" actual expected))
        (eqv? actual expected)
        ))
    )
  )


(module+ test
  (define/provide-test-suite ltl-tests
    ; (test-val)
    ; (test-not)
    ; (test-and)
    ; (test-or)
    ; (test-if)
    ; (test-next)
    (test-always)
    )
  (run-tests ltl-tests)
  )
