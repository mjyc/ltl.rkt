#lang rosette/safe

(require rosette/lib/match (only-in racket/base error))

(provide (all-defined-out))

(struct ltl-formula (type value) #:transparent)

(define (ltl-eval formula [lookup (lambda (dummy) #f)])
  (cond
    [(boolean? formula) formula]
    ; [(ltl-formula? formula)
    ;   (cond
    ;     [(equal? (ltl-formula-type formula) 'or) (define a #t) a]
    ;     [(equal? (ltl-formula-type formula) 'and) #f]
    ;     [else (error 'err "unknown ltl-formula ~a" formula)]
    ;     )
    ;   ]
    [(ltl-formula? formula)
      (define type (ltl-formula-type formula))
      (define value (ltl-formula-value formula))
      (cond
        [(equal? type 'not)
          (define f (ltl-eval value lookup))
          (match f
           [#t #f]
           [#f #t]
           [_ (ltl-formula 'not f)])
          ]
        [(equal? type 'and)
          (define fs (map (lambda (f) (ltl-eval f lookup)) value))
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
          (define fs (map (lambda (f) (ltl-eval f lookup)) value))
          (define fsWOBool (filter (lambda (f) (not (equal? f #f))) fs)) ; drops #f
          (cond
            [(andmap boolean? fsWOBool) (ormap identity fs)]
            [(member #t fsWOBool) #t] ; a disjunction contains a true is true
            [(= (length fsWOBool) 0) #f] ; an empty list is a disjunction of falses
            [(= (length fsWOBool) 1) (first fsWOBool)] ; 'or with one formula is just that formula
            [else (ltl-formula 'or fsWOBool)]
            )
          ]
        [(equal? type 'next)
         value
         ]
        [(equal? type 'always)
         (define f (ltl-eval value lookup))
         (cond
           [(boolean? f) (if f formula #f)]
         [else (ltl-formula 'and (list f formula))] ; add the unevaluated formula
         )
         ]
        [(equal? type 'eventually)
         (define f (ltl-eval value lookup))
         (cond
           [(boolean? f) (if f #t formula)]
         [else (ltl-formula 'or (list f formula))] ; add the unevaluated formula
         )
         ]
        [else (error 'err "unknown ltl-formula ~a" formula)]
        )]
    [else (lookup formula)]
    ))

(define (ltl-run formula stream)
  (define (step cur-value cur-formula)
    (displayln (list "====" cur-value))
    (define (lookup variable)
      (displayln "===")
      (print cur-value)
      (displayln "")
      (displayln variable)
      (displayln "")
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
