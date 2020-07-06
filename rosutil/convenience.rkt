#lang racket/base

(require racket/contract syntax/parse/define
         (prefix-in @ rosette/safe))

(provide
 (contract-out
  [fresh-symbolic (-> (or/c symbol? string?)
                      @solvable?
                      @constant?)]
  [concrete-head? (-> any/c
                      boolean?)]))

(define name-count (make-hash))

; instantiate a fresh symbolic that prints with the given name
;
; Rosette compares constant? terms with equal? (when going into the term cache etc)
; so we create a fresh uninterned symbol with a given string representation to create
; a fresh constant
(define (fresh-symbolic name type)
  (define sym-base
    (if (symbol? name)
        (symbol->string name)
        name))
  (define counter (hash-ref name-count sym-base 0))
  (hash-set! name-count sym-base (add1 counter))
  ; make names that look like the ones that come out of define-symbolic*, but a little different
  ; so x%0 instead of x$0
  (@constant (string->uninterned-symbol (format "~a%~a" sym-base counter)) type))

(define (concrete-head? expr)
  (not (or (@term? expr) (@union? expr))))
