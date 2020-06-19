#lang racket/base

(provide array-representation-vector
         overapproximate-symbolic-store-threshold
         overapproximate-symbolic-load-threshold)

; array representation:
; #t for vector
; #f for uninterpreted function
(define array-representation-vector (make-parameter #t))

; overapproximating stores to symbolic addresses:
; #f for no overapproximation
; n for overapproximating when the array size is >= n
;
; this only has an effect when (array-representation-vector) is #t
(define overapproximate-symbolic-store-threshold (make-parameter #f))

; overapproximating loads from symbolic addresses:
; #f for no overapproximation
; n for overapproximating when the array size is >= n
;
; this only has an effect when (array-representation-vector) is #t
(define overapproximate-symbolic-load-threshold (make-parameter #f))
