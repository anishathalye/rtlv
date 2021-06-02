#lang rosette/safe

(require
 syntax/parse/define
 (only-in racket/base error))

(provide with-pre)

(define (with-pre* pre expr-thunk)
  (define res
    (with-vc
      (begin
        (assume pre)
        (expr-thunk))))
  (when (failed? res)
    (error 'with-pre "evaluation terminated abnormally"))
  (define v (result-state res))
  (unless (unsat? (verify (begin (assume (vc-assumes v))
                                 (assert (vc-asserts v)))))
    (error 'with-pre "unable to discharge vc"))
  (result-value res))

(define-simple-macro (with-pre pre expr)
  (with-pre* pre (lambda () expr)))
