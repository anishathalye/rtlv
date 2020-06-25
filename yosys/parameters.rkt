#lang racket/base

(require racket/contract)

(provide array-representation-vector
         overapproximate-symbolic-store-threshold
         overapproximate-symbolic-load-threshold
         print-filter)

; array representation:
; #t for vector
; #f for uninterpreted function
(define array-representation-vector
  (make-parameter #t
                  (lambda (v)
                    (unless (boolean? v)
                      (raise-argument-error 'array-representation-vector
                                            "boolean?"
                                            v))
                    v)
                  'array-representation-vector))

; overapproximating stores to symbolic addresses:
; #f for no overapproximation
; n for overapproximating when the array size is >= n
;
; this only has an effect when (array-representation-vector) is #t
(define overapproximate-symbolic-store-threshold
  (make-parameter #f
                  (lambda (v)
                    (unless (or (not v)
                                (natural-number/c v))
                      (raise-argument-error 'overapproximate-symbolic-store-threshold
                                            "(or/c #f natural-number/c)"
                                            v))
                    v)
                  'overapproximate-symbolic-store-threhsold))

; overapproximating loads from symbolic addresses:
; #f for no overapproximation
; n for overapproximating when the array size is >= n
;
; this only has an effect when (array-representation-vector) is #t
(define overapproximate-symbolic-load-threshold
  (make-parameter #f
                  (lambda (v)
                    (unless (or (not v)
                                (natural-number/c v))
                      (raise-argument-error 'overapproximate-symbolic-load-threshold
                                            "(or/c #f natural-number/c)"
                                            v))
                    v)
                  'overapproximate-symbolic-load-threhsold))

; filter what fields are included in the printed representation of a struct
; #f: no filter
; regexp?: to match the string representation of the symbol
; symbol? -> boolean?: filter function
(define print-filter
  (make-parameter (lambda (s) #t)
                  (lambda (v)
                    (unless (or (not v)
                                (regexp? v)
                                ((procedure-arity-includes/c 1) v))
                      (raise-argument-error 'print-filter
                                            "(or/c #f regexp? (procedure-arity-includes/c 1))"
                                            v))
                    (cond
                      [(not v) (lambda (s) #t)]
                      [(regexp? v) (lambda (s) (regexp-match v (symbol->string s)))]
                      [else v]))
                  'print-filter))
