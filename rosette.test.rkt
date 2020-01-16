#lang rosette/safe

(require rackunit rackunit/text-ui "./rosette.rkt")

(define (run formula stream)
  (define (step cur-value cur-formula)
    (define (lookup variable)
      (equal? variable cur-value)
      )
    (ltl-eval cur-formula lookup)
    )
  (foldl
    step
    formula
    stream)
  )

(define (test-next)
  (test-case
    "test-next"
    (define-symbolic sb boolean?)

    (define stream
      (if sb (list 'b 'b 'b) (list 'b 'b 'a)))
    (define formula (ltl-formula 'next (ltl-formula 'next 'a)))

    (define sol (solve
      (assert
        (equal?
          (run formula stream)
          #t)
        )))
    (check-true (sat? sol))
    (check-equal? (evaluate sb sol) #f)
    )
  )

(define (test-always)
  (test-case
    "test-always"
    (define-symbolic sb boolean?)

    (define stream
      (if sb (list 'a 'a 'b) (list 'a 'a 'a)))
    (define formula (ltl-formula 'always 'a))

    (define sol (solve
      (assert
        (equal?
          (run formula stream)
          (ltl-formula 'always 'a))
        )))
    (check-true (sat? sol))
    (check-equal? (evaluate sb sol) #f)
    )
  )

(define (test-eventually)
  (test-case
    "test-eventually"
    (define-symbolic sb boolean?)

    (define stream
      (if sb (list 'b 'b 'b) (list 'b 'b 'a)))
    (define formula (ltl-formula 'eventually 'a))

    (define sol (solve
      (assert
        (equal?
          (run formula stream)
          #t)
        )))
    (check-true (sat? sol))
    (check-equal? (evaluate sb sol) #f)
    )
  )

(module+ test
  (define/provide-test-suite ltl-tests
    (test-next)
    (test-always)
    (test-eventually)
    )
  (run-tests ltl-tests)
  )
