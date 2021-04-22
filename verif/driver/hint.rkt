#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide
 (struct-out hint)
 (struct-out merge)
 (struct-out fixpoint)
 (struct-out debug)
 (struct-out debug*)
 (struct-out circuit-hint)
 (struct-out concretize)
 (struct-out overapproximate)
 make-hintdb)

(struct hint ())

(struct merge hint (key))

(struct fixpoint hint (setup-cycles abstract-field-filter cycle-length step-hints))

(struct debug hint (fn))

(struct debug* hint (fn))

(struct circuit-hint hint ())

(struct concretize circuit-hint (field-filter))

(struct overapproximate circuit-hint (field-filter))

(define-syntax make-hintdb
  (syntax-parser
    [(_ [name:id hint:expr] ...)
     #'(make-hash (list (cons 'name hint) ...))]))
