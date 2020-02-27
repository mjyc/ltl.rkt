#lang rosette/safe

(require rosette/lib/match (only-in racket/base error) "./logger.rkt")

(provide (all-defined-out))

(struct ltl-formula (type value) #:transparent)

(define (ltl-eval formula [lookup (lambda (dummy) #f)] [terminal #f])
  (cond
    [(boolean? formula) formula]
    [(ltl-formula? formula)
      (define type (ltl-formula-type formula))
      (define value (ltl-formula-value formula))
      (cond
        [(equal? type 'not)
          (define f (ltl-eval value lookup terminal))
          (match f
            [#t #f]
            [#f #t]
            [_ (ltl-formula 'not f)])
          ]
        [(equal? type 'and)
          (define fs (map (lambda (f) (ltl-eval f lookup terminal)) value))
          (define fsWOBool (filter (lambda (f) (not (equal? f #t))) fs)) ; drops #t
          (cond
            [(andmap boolean? fsWOBool) (andmap identity fsWOBool)]
            [(member #f fsWOBool) #f] ; a conjunction contains a false is false
            [(= (length fsWOBool) 0) #t] ; an empty list is a conjunction of trues
            [(= (length fsWOBool) 1) (first fsWOBool)] ; 'and with one formula is just that formula
            [else (ltl-formula 'and fsWOBool)]
            )
          ]
        [(equal? type 'or)
          (define fs (map (lambda (f) (ltl-eval f lookup terminal)) value))
          (define fsWOBool (filter (lambda (f) (not (equal? f #f))) fs)) ; drops #f
          (cond
            [(andmap boolean? fsWOBool) (ormap identity fs)]
            [(member #t fsWOBool) #t] ; a disjunction contains a true is true
            [(= (length fsWOBool) 0) #f] ; an empty list is a disjunction of falses
            [(= (length fsWOBool) 1) (first fsWOBool)] ; 'or with one formula is just that formula
            [else (ltl-formula 'or fsWOBool)]
            )
          ]
        [(equal? type 'if)
          (unless (equal? (length value) 2)
            (error 'ltl-eval "invalid \"value\" argument ~a" value)
            )
          (define f (ltl-formula 'or (list (ltl-formula 'not (first value)) (second value))))
          (ltl-eval f lookup terminal)
          ]
        [(equal? type 'next)
          value
          ]
        [(equal? type 'always)
          (define f (ltl-eval value lookup terminal))
          (cond
            [(boolean? f) (if f (if terminal #t formula) #f)]
            [else (if terminal #t (ltl-formula 'and (list f formula)))] ; add the unevaluated formula
            )
          ]
        [(equal? type 'eventually)
          (define f (ltl-eval value lookup terminal))
          (cond
            [(boolean? f) (if f #t (if terminal #f formula))]
            [else (if terminal #f (ltl-formula 'or (list f formula)))] ; add the unevaluated formula
            )
          ]
        [else (error 'err "unknown ltl-formula ~a" formula)]
        )]
    [else (if terminal formula (lookup formula))]
    ))

(define (ltl-run formula stream)
  (define (step cur-value cur-formula)
    (define (lookup variable)
      (equal? variable cur-value)
      )
    (ltl-eval cur-formula lookup)
    )
  (define f
    (foldl
      step
      formula
      stream))
  (ltl-eval f identity #t)
  )
