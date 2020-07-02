#lang racket/base

(require racket/contract syntax/parse/define
         (prefix-in @ rosette/safe))

(provide
 fresh-symbolic
 (contract-out
  [concrete-head? (-> any/c
                      boolean?)]))

(define-simple-macro (fresh-symbolic name:id type:expr)
  (@let () (@define-symbolic* name type) name))

(define (concrete-head? expr)
  (not (or (@term? expr) (@union? expr))))
