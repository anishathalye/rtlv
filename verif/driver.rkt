#lang racket/base

(require
 racket/provide
 racket/stxparam
 (prefix-in @ rosette/safe)
 (for-syntax racket/base racket/syntax syntax/parse)
 (only-in "interpreter.rkt" basic-value? closure make-assoc assoc-extend make-interpreter))

(provide (rename-out [$#%module-begin #%module-begin])
         #%top-interaction #%app #%datum #%datum #%top
         (rename-out [$define define])
         ;; some of the simple builtins from interpreter
         void void?
         printf println
         equal?
         cons car cdr null? list? list
         not
         + - * quotient modulo zero? add1 sub1 abs max min < <= > >= expt integer?
         (filtered-out
          (lambda (name) (substring name 1))
          (combine-out
           @bv @bv?
           @bveq @bvslt @bvult @bvsle @bvule @bvsgt @bvugt @bvsge @bvuge
           @bvnot @bvand @bvor @bvxor @bvshl @bvlshr @bvashr
           @bvneg @bvadd @bvsub @bvmul @bvsdiv @bvudiv @bvsrem @bvurem @bvsmod
           @concat @extract @sign-extend @zero-extend @bitvector->integer @bitvector->natural @integer->bitvector
           @bit @lsb @msb @bvzero? @bvadd1 @bvsub1 @bvsmin @bvumin @bvsmax @bvumax @bvrol @bvror @rotate-left @rotate-right @bitvector->bits @bitvector->bool @bool->bitvector)))

(define-syntax $define
  (syntax-parser
    ;; function definition
    [(_ (function-name:id formals:id ...) body:expr ...+)
     #'(cons 'function-name
             (closure '(lambda (formals ...) body ...) (make-assoc)))]
    ;; value definition
    [(_ value-name:id body:expr)
     #'(cons 'value-name (let ([v body])
                           (if (basic-value? v) v (error 'value-name "must evaluate to a value"))))]))


(define-syntax ($#%module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #:with interpreter-factory (format-id stx "interpreter-factory")
     #'(#%module-begin
        (define global-bindings
          (for/fold ([env (make-assoc)])
                    ([def (list form ...)])
            (assoc-extend env (car def) (cdr def))))
        (define ((interpreter-factory metadata) expr initial-circuit)
          (make-interpreter expr global-bindings initial-circuit metadata))
        (provide interpreter-factory))]))
