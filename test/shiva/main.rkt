#lang racket

(require shiva
         (prefix-in @ rosette/safe)
         rackunit)

(test-case "with-invariants"
  (@struct mod (x y) #:transparent)
  (define (symbolic-mod)
    (@define-symbolic* x y (@bitvector 32))
    (mod x y))
  (define (invariant m)
    (@equal? (mod-x m) (@bv #x1337 32)))
  (define m
    (with-invariants mod (symbolic-mod) invariant))
  (check-equal? (mod-x m) (@bv #x1337 32))
  (check-pred @constant? (mod-y m)))
