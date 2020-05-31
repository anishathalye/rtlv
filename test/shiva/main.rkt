#lang racket

(require shiva
         (prefix-in r: rosette)
         rackunit)

(test-case "with-invariants"
  (r:struct mod (x y) #:transparent)
  (define (symbolic-mod)
    (r:define-symbolic* x y (r:bitvector 32))
    (mod x y))
  (define (invariant m)
    (r:equal? (mod-x m) (r:bv #x1337 32)))
  (define m
    (with-invariants mod (symbolic-mod) invariant))
  (check-equal? (mod-x m) (r:bv #x1337 32))
  (check-pred r:constant? (mod-y m)))
