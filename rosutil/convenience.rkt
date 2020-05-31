#lang rosette/safe

(require syntax/parse/define)

(define-simple-macro (fresh-symbolic name:id type:expr)
  (let () (define-symbolic* name type) name))
(provide fresh-symbolic)

(define (concrete-head? expr)
  (not (or (term? expr) (union? expr))))
(provide concrete-head?)
