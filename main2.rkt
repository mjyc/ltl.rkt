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

(define (ltlinterpret formula [lookup (lambda (dummy) #f)])
  (match formula
    [(ltlval a1)
      (cond
        [(boolean? a1) a1]
        [else (lookup formula)]
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
    [(ltlalways a1) '()]
    [(ltleventually a1) '()]
    [(ltluntil a1 a2) '()]
    )
  )
