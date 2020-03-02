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

(define (test-always)
  (test-case
    "test-always"
    (define-symbolic sb boolean?)

    (define stream
      (if sb (list 'a 'a 'b) (list 'a 'a 'a)))
    (define formula (ltlalways (ltlval 'a)))

    (define sol (solve
      (assert
        (equal?
          (ltleval formula stream)
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
    (define formula (ltleventually (ltlval 'a)))

    (define sol (solve
      (assert
        (equal?
          (ltleval formula stream)
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
    (define formula
      (ltleventually (ltland (ltlval 'b) (ltleventually (ltlval 'd)))))

    (define sol (solve
      (assert
        (equal?
          (ltleval formula stream)
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
    (test-eventually-nested)
    )
  (run-tests ltl-tests)
  )
