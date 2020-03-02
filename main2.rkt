#lang racket/base

(require racket/function racket/list racket/match)

(provide (all-defined-out))

(struct ltlval (a1) #:transparent)
(struct ltlnot (a1) #:transparent)
(struct ltland (a1 a2) #:transparent)
(struct ltlor (a1 a2) #:transparent)
(struct ltlif (a1 a2) #:transparent)
(struct ltlnext (a1) #:transparent)
(struct ltlalways (a1) #:transparent)
(struct ltleventually (a1) #:transparent)
(struct ltluntil (a1 a2) #:transparent)

(define (ltlinterpret formula [lookup null])
  (match formula
    [(ltlval a1)
      (cond
        [(boolean? a1) a1]
        [else (if (null? lookup) a1 (lookup a1))]
        )
      ]
    [(ltlnot a1)
      (define f (ltlinterpret a1 lookup))
      (cond
        [(boolean? f) (not f)]
        [else (ltlnot f)]
        )
      ]
    [(ltland a1 a2)
      (define f1 (ltlinterpret a1 lookup))
      (define f2 (ltlinterpret a2 lookup))
      (cond
        [(or (equal? f1 #f) (equal? f2 #f)) #f]
        [(equal? f1 #t) f2]
        [(equal? f2 #t) f1]
        [else (ltland f1 f2)]
        )
      ]
    [(ltlor a1 a2)
      (define f1 (ltlinterpret a1 lookup))
      (define f2 (ltlinterpret a2 lookup))
      (cond
        [(or (equal? f1 #t) (equal? f2 #t)) #t]
        [(equal? f1 #f) f2]
        [(equal? f2 #f) f1]
        [else (ltlor f1 f2)]
        )
      ]
    [(ltlif a1 a2)
      (ltlinterpret (ltlor (ltlnot a1) a2) lookup)
      ]
    [(ltlnext a1)
      a1
      ]
    [(ltlalways a1)
      (define f (ltlinterpret a1 lookup))
      (cond
        [(null? lookup) #t]
        [(equal? f #f) #f]
        [(equal? f #t) formula]
        [else (ltland f formula)]
        )
      ]
    [(ltleventually a1)
      (define f (ltlinterpret a1 lookup))
      (cond
        [(null? lookup) #f]
        [(equal? f #t) #t]
        [(equal? f #f) formula]
        [else (ltlor f formula)]
        )
      ]
    [(ltluntil a1 a2) '()]
    )
  )

(define (ltleval formula stream)
  (define (step cur-value cur-formula)
    (define (lookup variable)
      (equal? variable cur-value)
      )
    (ltlinterpret
      (if (boolean? cur-formula) (ltlval cur-formula) cur-formula)
      lookup)
    )
  (define f
    (foldl
      step
      formula
      stream))
  (ltlinterpret (if (boolean? f) (ltlval f) f))
  )
