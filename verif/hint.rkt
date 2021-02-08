#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide
 (struct-out hint)
 (struct-out merge)
 (struct-out concretize)
 (struct-out fixpoint)
 make-hintdb)

(struct hint ())

(struct merge hint ())

(struct concretize hint (field-filter))

(struct fixpoint hint (setup-cycles abstract-field-filter cycle-length))

(define-syntax make-hintdb
  (syntax-parser
    [(_ [name:id hint:expr] ...)
     #'(make-hash (list (cons 'name hint) ...))]))
