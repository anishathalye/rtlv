#lang rosette/safe

(require syntax/parse/define)

(provide fresh-symbolic concrete-head?)

(define-simple-macro (fresh-symbolic name:id type:expr)
  (let () (define-symbolic* name type) name))

(define (concrete-head? expr)
  (not (or (term? expr) (union? expr))))
