# LTL.rkt

[Linear temporal logic](https://en.wikipedia.org/wiki/Linear_temporal_logic) for [Racket](https://racket-lang.org/). For now, it only implements `always` and `eventually` temporal operators.

# Getting started

```
cd path/to/ltl
raco pkg install ltl  # install ltl as a local package
raco test -p ltl  # run test
```

# Example

```
#racket

(require ltl)

(define formula (ltl-formula 'and (list
  'a
  (ltl-formula 'next 'b)
  )))

(define f1 (ltl-eval formula (lambda (v) (if (eqv? v 'a) #t #f))))
(displayln f1) ; b
(define f2 (ltl-eval f1 (lambda (v) (if (eqv? v 'b) #t #f))))
(displayln f2) ; #t
```

## Acknowledgement

Thanks [Christian Fritz](https://github.com/chfritz) for introducing me to linear temporal logic.
