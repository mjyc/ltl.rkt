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

(define (test-ltleval)
  (test-case
    "test-ltleval"
    (define-symbolic sb boolean?)

    (define stream1 (list 'a 'b 'a 'a))
    (define formula1
      (ltleventually (ltland (ltlval 'b) (ltleventually (ltlval 'd)))))
    (define actual1 (ltleval formula1 stream1))
    (check-equal? actual1 #f)

    (define stream2 (list 'a 'a 'a 'a))
    (define formula2 (ltlalways (ltlval 'a)))
    (define actual2 (ltleval formula2 stream2))
    (check-equal? actual2 #t)
    )
  )

(module+ test
  (define/provide-test-suite ltl-tests
    (test-next)
    (test-always)
    (test-eventually)
    (test-eventually-nested)
    (test-ltleval)
    )
  (run-tests ltl-tests)
  )
