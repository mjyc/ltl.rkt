#lang racket/base

(require racket/list)
(require racket/match)

(provide (all-defined-out))

(struct ltl-formula (type value) #:transparent)

(define (ltl-eval formula [lookup (lambda (dummy) #f)])
  ; (displayln (list "formula" formula))
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
       (define fsWOBool (filter (lambda (f) (not (equal? f #t))) fs)) ; drops #t
       (cond
         [(index-of fs #f) #f] ; a conjunction contains a false is false
         [(= (length fsWOBool) 0) #t] ; an empty conjunction is a conjunction of trues
         [(= (length fsWOBool) 1) (first fsWOBool)] ; 'and with one formula is just that formula
         [else (ltl-formula 'and fsWOBool)]
         )
       ]
      [(ltl-formula 'or value)
       (define fs (map (lambda (f) (ltl-eval f lookup)) value))
       (define fsWOBool (filter (lambda (f) (not (equal? f #f))) fs)) ; drops #f
       (cond
         [(index-of fs #t) #t] ; a disjunction contains a true is true
         [(= (length fsWOBool) 0) #f] ; an empty disjunction is a conjunction of falses
         [(= (length fsWOBool) 1) (first fsWOBool)] ; 'or with one formula is just that formula
         [else (ltl-formula 'and fsWOBool)]
         )
       ]
      [(ltl-formula 'next value)
       value
       ]
      [(ltl-formula 'always value)
       (define f (ltl-eval value lookup))
       (match f
         [#f #f] ; always false is false
         [#t formula]
         [_ (ltl-formula 'and (list f formula))] ; add the unevaluated formula
         )
       ]
      [(ltl-formula 'eventually value)
       (define f (ltl-eval value lookup))
       (match f
         [#t #t] ; eventually true is true
         [#f formula]
         [_ (ltl-formula 'or (list f formula))] ; add the unevaluated formula
         )
       ]
      [_ (error 'err "unknown ltl-formula ~a" formula)]
      )]
    [else (error 'err "unknown formula ~a" formula)]
    ))
