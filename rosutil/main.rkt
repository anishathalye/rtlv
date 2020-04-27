#lang rosette/safe

(require (prefix-in racket: racket)
         syntax/parse/define)

(define-simple-macro (fresh-symbolic name:id type:expr)
  (let () (define-symbolic* name type) name))
(provide fresh-symbolic)
