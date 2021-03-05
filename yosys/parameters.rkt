#lang racket/base

(require racket/contract racket/string)

(provide array-representation-vector
         overapproximate-symbolic-store-threshold
         overapproximate-symbolic-load-threshold
         field-filter
         to-field-filter
         (contract-out
          [filter/not
           (-> filter/c
               (-> symbol? any))]
          [filter/or
           (->* () #:rest (listof filter/c)
                (-> symbol? any))]))

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
                    v)))

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
                    v)))

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
                    v)))

(define filter/c
  (or/c boolean? string? regexp? (-> symbol? any)))

(define (to-field-filter v)
  (cond
    [(boolean? v) (lambda (s) v)]
    [(string? v) (lambda (s) (string-contains? (symbol->string s) v))]
    [(regexp? v) (lambda (s) (regexp-match v (symbol->string s)))]
    [else v]))

(define (filter/not filter)
  (define filter-function (to-field-filter filter))
  (lambda (s) (not (filter-function s))))

(define (filter/or . filters)
  (define filter-functions (map to-field-filter filters))
  (lambda (s)
    (for/or ([fn (in-list filter-functions)])
      (fn s))))

; filter what fields are included in the printed representation of a struct
; #f: no filter
; string?: to require the string representation of the symbol contains a given substring
; regexp?: to match the string representation of the symbol
; symbol? -> any: filter function
(define field-filter
  (make-parameter (to-field-filter #t)
                  (lambda (v)
                    (unless (or (not v)
                                (string? v)
                                (regexp? v)
                                ((procedure-arity-includes/c 1) v))
                      (raise-argument-error 'field-filter
                                            "(or/c #f string? regexp? (procedure-arity-includes/c 1))"
                                            v))
                    (to-field-filter v))))
