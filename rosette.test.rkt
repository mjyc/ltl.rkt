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
    (logger-debug "---")

    (define-symbolic sb boolean?)

    ; (define stream
    ;   (list 'a 'b 'c (if sb 'd 'a)))
    (define stream
      (list (if sb 'b 'a) 'd))
    (define formula (ltl-formula 'eventually
      (ltl-formula 'and
        (list 'b (ltl-formula 'eventually 'd)))))

    ; (define outf (ltl-run formula stream))
    ; (displayln "--done--")
    ; (print outf)
    ; (displayln "")
    ; (displayln (ltl-formula? outf))
    ; (print (ltl-formula-type outf))
    ; (displayln "")
    ; (print (ltl-formula-value outf))
    ; (displayln "")
    (define sol (solve
      (assert
        (equal?
          (ltl-run formula stream)
          #t)
          ; (ltl-formula-type (ltl-run formula stream))
          ; 'or)
          ; (first (ltl-formula-value outf))
          ; #t)
          )))
    (check-true (sat? sol))
    (check-equal? (evaluate sb sol) #t)
    )
  )

(define (test3)
  (test-case
    "test3"
    (define-symbolic sb boolean?)

    (define formula (ltl-formula (if sb 'and 'or) (list #t #f)))

    (define sol (solve
      (assert
        (equal?
          (ltl-eval formula)
          #t)
        )))
    (check-true (sat? sol))
    (logger-debug (evaluate sb sol))
    ; (check-equal? (evaluate sb sol) #f)
    )
  )

(module+ test
  (define/provide-test-suite ltl-tests
    ; (test-next)
    ; (test-always)
    ; (test-eventually)
    (test-eventually2)
    ; (test3)
    )
  (run-tests ltl-tests)
  )
