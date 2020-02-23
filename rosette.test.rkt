#lang rosette/safe

(require rackunit rackunit/text-ui
  "./rosette.rkt"
  (prefix-in logger- "./logger.rkt")
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
          (ltl-run formula stream)
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
          (ltl-run formula stream)
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
          (ltl-run formula stream)
          #t)
        )))
    (check-true (sat? sol))
    (check-equal? (evaluate sb sol) #f)
    )
  )

(define (test-eventually2)
  (test-case
    "test-eventually2"
    (define-symbolic sb boolean?)

    (define stream
      (list 'a (if sb 'b 'a) 'c 'd))
    (define formula (ltl-formula 'eventually
      (ltl-formula 'and
        (list 'b (ltl-formula 'eventually 'd)))))

    (define sol (solve
      (assert
        (equal?
          (ltl-run formula stream)
          #t)
        )))
    (check-true (sat? sol))
    (check-equal? (evaluate sb sol) #t)
    )
  )

(module+ test
  (define/provide-test-suite ltl-tests
    (test-next)
    (test-always)
    (test-eventually)
    (test-eventually2)
    )
  (run-tests ltl-tests)
  )
