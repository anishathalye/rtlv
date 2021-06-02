#lang racket/base

(require
 (prefix-in @ rosette/safe)
 (for-syntax racket/base syntax/parse))

(provide
 (struct-out hint)
 (struct-out merge)
 (struct-out fixpoint)
 (struct-out case-split)
 case-split*
 (struct-out debug)
 (struct-out debug*)
 (struct-out circuit-hint)
 (struct-out concretize)
 (struct-out overapproximate)
 make-hintdb)

(struct hint ())

(struct merge hint (key))

(struct fixpoint hint (setup-cycles abstract-field-filter cycle-length step-hints))

(struct case-split hint (splits exhaustive))

(define (case-split* splits)
  (case-split
   (let loop ([acc '()]
              [ps splits]
              [pc #t])
     (if (null? ps)
         (cons pc acc)
         (loop
          (cons (@&& pc (car ps)) acc)
          (cdr ps)
          (@&& pc (@not (car ps))))))
   #t))

(struct debug hint (fn))

(struct debug* hint (fn))

(struct circuit-hint hint ())

(struct concretize circuit-hint (field-filter))

(struct overapproximate circuit-hint (field-filter))

(define-syntax make-hintdb
  (syntax-parser
    [(_ [name:id hint:expr] ...)
     #'(make-hash (list (cons 'name hint) ...))]))
