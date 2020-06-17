#lang racket

(require (only-in rosette bv? expression constant union)
         (only-in rosette/base/core/term type-deconstruct get-type typed?))

(provide value-size)

; calculate a Rosette value's size
;
; the value returned by this function should match what you would
; get from calling render-value/snip, fully expand the value,
; and then counting the number of boxes
(define (value-size val)
  (define (value-size-sum vs)
    (for/fold ([sum 0])
              ([v (in-list vs)])
      (+ sum (value-size v))))
  ; mirrors the structure used in render-value/snip
  ; https://github.com/emina/rosette/blob/master/rosette/lib/value-browser.rkt
  (match val
    [(constant id type) 1]
    [(union gvs)
     (for/fold ([sum 1])
               ([child (in-list gvs)])
       (define condition (car child))
       (define value (cdr child))
       (+ sum (value-size condition) (value-size value)))]
    [(expression op child ...)
     (+ 1 (value-size-sum child))]
    [(list child ...)
     (+ 1 (value-size-sum child))]
    [(vector child ...)
     (+ 1 (value-size-sum child))]
    [(? box?)
     (+ 1 (value-size-sum (unbox val)))]
    [(cons a b)
     (+ 1 (value-size a) (value-size b))]
    [(? integer?) 1]
    [(? real?) 1]
    [(? boolean?) 1]
    [(? typed?)
     (define t (get-type val))
     (match (type-deconstruct t val)
       [(list (== val))
        ;; typed value
        (match val
          [(? bv?) 1]
          [(? procedure?) 1]
          [_
           ;; this should be a dead code in principle
           0]
          [vs
           (+ 1 (value-size-sum vs))])])]
    ;; a struct could have prop:procedure, so this test should
    ;; follow the struct test
    [(? procedure?) 1]
    [_
     ; other
     1]))
