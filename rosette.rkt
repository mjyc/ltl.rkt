; using '#lang rosette/safe' need (require rosette/lib/match) and lifting symbol? and index-of
#lang rosette

(provide (all-defined-out))

(struct ltl-formula (type value) #:transparent)

(define (ltl-eval formula [lookup (lambda (dummy) #f)])
  (cond
    [(boolean? formula) formula]
    [(symbol? formula) (lookup formula)]
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
       (cond
         [(andmap boolean? fs) (andmap identity fs)]
         [(= (length fs) 1) (first fs)] ; 'and with one formula is just that formula
         [else (ltl-formula 'and fs)]
         )
       ]
      [(ltl-formula 'or value)
        (define fs (map (lambda (f) (ltl-eval f lookup)) value))
        (cond
          [(andmap boolean? fs) (ormap identity fs)]
          [(= (length fs) 1) (first fs)] ; 'or with one formula is just that formula
          [else (ltl-formula 'and fs)]
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
    [else (error 'err "unknown formula ~a" formula)]
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
