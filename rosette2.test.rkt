#lang rosette/safe

(require rackunit rackunit/text-ui "./rosette2.rkt")

(define (test-next)
  (test-case
    "test-next"
    (define-symbolic sb boolean?)

    (define stream
      (if sb (list 'b 'b 'b) (list 'b 'b 'a)))
    (define formula (ltlnext (ltlnext (ltlval 'a))))

    (define sol
      (solve
        (assert
          (equal? (ltleval formula stream) #t)
          )
        )
      )
    (check-true (sat? sol))
    (check-equal? (evaluate sb sol) #f)
    )
  )

(module+ test
  (define/provide-test-suite ltl-tests
    (test-next)
    )
  (run-tests ltl-tests)
  )
