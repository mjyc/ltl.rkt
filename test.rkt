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

(define (test-if)
  (test-case
    "test-if"
    (check-property
      (property (
        [bool1 arbitrary-boolean]
        [bool2 arbitrary-boolean]
        )
      (eqv? (ltl-eval (ltl-formula 'if (list bool1 bool2))) (or (not bool1) bool2))
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

(define (test-eventually-hash)
  (test-case
    "test-eventually-hash"
    (define stream
      (list
        #hash((a . #f) (b . #f))
        #hash((a . #t) (b . #f))
        #hash((a . #f) (b . #t))
        ))
    (define formula (ltl-formula 'eventually
      (ltl-formula 'and
        (list (list 'a #t) (ltl-formula 'eventually (list 'b #t))))))
    (define actual (ltl-run formula stream))
    (check-equal? actual #t)
    )
  )

(define (test-until)
  (test-case
    "test-until"
    (define stream
      (list
        #hash((a . #t) (b . #f))
        #hash((a . #t) (b . #f))
        #hash((a . #t) (b . #f))
        #hash((a . #f) (b . #t))
        ))
    (define formula (ltl-formula 'until (list (list 'a #t) (list 'b #t))))
    (define actual (ltl-run formula stream))
    (check-equal? actual #t)

    (define stream2
      (list
        #hash((a . #t) (b . #f))
        #hash((a . #t) (b . #f))
        ))
    (define formula2 (ltl-formula 'until (list (list 'a #t) (list 'b #t))))
    (define actual2 (ltl-run formula2 stream2))
    (check-equal? actual2 #f)
    )
  )

(module+ test
  (define/provide-test-suite ltl-tests
    (test-boolean)
    (test-not)
    (test-and)
    (test-or)
    (test-if)
    (test-next)
    (test-always)
    (test-eventually)
    (test-eventually-hash)
    (test-until)
    )
  (run-tests ltl-tests)
  )
