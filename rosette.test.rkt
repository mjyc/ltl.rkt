#lang rosette/safe

(require rackunit rackunit/text-ui
  "./rosette.rkt"
  "./logger.rkt"
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
          #t)
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

(define (test-eventually-nested)
  (test-case
    "test-eventually-nested"
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

(define (test-ltl-run)
  (test-case
    "test-ltl-run"
    (define-symbolic sb boolean?)

    (define stream1
      (list 'a 'b 'a 'a))
    (define formula1 (ltl-formula 'eventually
      (ltl-formula 'and
        (list 'b (ltl-formula 'eventually 'd)))))
    (define actual1 (ltl-run formula1 stream1))
    (check-equal? actual1 #f)

    (define stream2
      (list 'a 'a 'a 'a))
    (define formula2 (ltl-formula 'always
      'a))
    (define actual2 (ltl-run formula2 stream2))
    (check-equal? actual2 #t)
    )
  )

(module+ test
  (define/provide-test-suite ltl-tests
    (test-next)
    (test-always)
    (test-eventually)
    (test-eventually-nested)
    (test-ltl-run)
    )
  (run-tests ltl-tests)
  )
