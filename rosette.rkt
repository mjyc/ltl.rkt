#lang rosette/safe

(require rosette/lib/match (only-in racket/base error))

(provide (all-defined-out))

(struct ltl-formula (type value) #:transparent)

(define (ltl-eval formula [lookup (lambda (dummy) #f)])
  (cond
    [(boolean? formula) formula]
    [(ltl-formula? formula) (match formula
      [(ltl-formula 'not value)
       (define f (ltl-eval value lookup))
       (match f
         [#t #f]
         [#f #t]
         [_ (ltl-formula 'not f)])
       ]
      [(ltl-formula 'and value)
       (define fs (map (lambda (f) (ltl-eval f lookup)) value))
       (define fsWOBool (filter (lambda (f) (not (equal? f #t))) fs)) ; drops #t
       (cond
         [(andmap boolean? fsWOBool) (andmap identity fsWOBool)]
         [(member #f fsWOBool) #f] ; a conjunction contains a false is false
         [(= (length fsWOBool) 0) #t] ; an empty conjunction is a conjunction of trues
         [(= (length fsWOBool) 1) (first fsWOBool)] ; 'and with one formula is just that formula
         [else (ltl-formula 'and fsWOBool)]
         )
       ]
      [(ltl-formula 'or value)
        (define fs (map (lambda (f) (ltl-eval f lookup)) value))
        (define fsWOBool (filter (lambda (f) (not (equal? f #f))) fs)) ; drops #f
        (cond
          [(andmap boolean? fsWOBool) (ormap identity fs)]
          [(member #t fsWOBool) #t] ; a disjunction contains a true is true
          [(= (length fsWOBool) 0) #f] ; an empty disjunction is a conjunction of falses
          [(= (length fsWOBool) 1) (first fsWOBool)] ; 'or with one formula is just that formula
          [else (ltl-formula 'or fs)]
          )
        ]
      [(ltl-formula 'next value)
       value
       ]
      [(ltl-formula 'always value)
       (define f (ltl-eval value lookup))
       (cond
         [(boolean? f) (if f formula #f)]
         [else (ltl-formula 'and (list f formula))] ; add the unevaluated formula
         )
       ]
      [(ltl-formula 'eventually value)
       (define f (ltl-eval value lookup))
       (cond
         [(boolean? f) (if f #t formula)]
         [else (ltl-formula 'or (list f formula))] ; add the unevaluated formula
         )
       ]
      [_ (error 'err "unknown ltl-formula ~a" formula)]
      )]
    [else (lookup formula)]
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
  f
  )
