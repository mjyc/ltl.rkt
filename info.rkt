#lang info
(define collection "ltl")
(define deps '("base"))
(define build-deps '("rackunit-lib" "quickcheck"))
(define pkg-desc "Linear Temporal Logic for Racket")
(define version "0.0")
(define pkg-authors '(mjyc))
(define test-omit-paths '(#rx"^((?!test\\.rkt).)*$"))
