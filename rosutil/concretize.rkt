#lang racket

(require (prefix-in r: rosette)
         (for-syntax syntax/parse
                     racket/syntax))

; Note: these functions are not general-purpose -- it is in general,
; NOT safe to use them in arbitrary Rosette programs. At best,
; they might _increase_ the size of terms; at worst, they may produce
; incorrect results.
;
; We use these only in conjunction with yosys, where we have a situation
; where the assertion store is empty, and there is no mutation everywhere
; (it's all pure functional code), so I think this is okay.

(define (concretize term [error-on-failure #f])
  (let/ec return
    (define vars (r:symbolics term))
    (when (empty? vars) (return term))
    (define model (r:solve #t))
    (when (not (r:sat? model))
      (error "can't find single concrete state"))
    (define term-concrete (r:evaluate term (r:complete-solution model (r:symbolics term))))
    (define must-be-same
      (r:unsat?
       (r:verify
        (r:assert
         (r:equal? term term-concrete)))))
    (cond
      [must-be-same term-concrete]
      [(not error-on-failure) term]
      [else (error 'concretize "failed to concretize term")])))
(provide concretize)

(define-syntax (concretize-fields stx)
  (syntax-parse stx
    [(_ struct-id:id struct-elem:expr (field-name:id ...))
     #:with (getter-name ...) (for/list ([field-name (syntax->list #'(field-name ...))])
                                (format-id stx "~a-~a" (syntax-e #'struct-id) (syntax-e field-name)))
     #'(begin
         (define struct-v struct-elem)
         (struct-copy
          struct-id
          struct-v
          [field-name (concretize (getter-name struct-v))] ...))]))
(provide concretize-fields)