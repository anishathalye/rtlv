#lang racket/base

(require shiva
         rosutil
         (prefix-in @ rosette/safe)
         rackunit)

(test-case "with-invariants"
  (@struct mod (x y) #:transparent
           #:methods gen:addressable
           [(define (fields _) '(x y))
            (define (get-field m name)
              (case name
                [(x) (mod-x m)]
                [(y) (mod-y m)]))
            (define (map-fields m f)
              (mod (f 'x (mod-x m))
                   (f 'y (mod-y m))))])
  (define (symbolic-mod)
    (@define-symbolic* x y (@bitvector 32))
    (mod x y))
  (define (invariant m)
    (@equal? (mod-x m) (@bv #x1337 32)))
  (define m
    (with-invariants (symbolic-mod) invariant))
  (check-equal? (mod-x m) (@bv #x1337 32))
  (check-pred @constant? (mod-y m)))
