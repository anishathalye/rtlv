#lang racket/base

(require racket/list racket/contract
         yosys
         (prefix-in @ rosette/safe)
         (for-syntax racket/base syntax/parse racket/syntax))

(provide
 concretize-fields
 (contract-out
  [concretize (->* (any/c)
                   (boolean?)
                   any)]
  [concretize-all-fields (-> yosys-module?
                             yosys-module?)]
  [all-values (->i ([term any/c])
                   (#:limit [limit (or/c boolean? natural-number/c)])
                   [result (limit)
                           (and/c list?
                                  (if (unsupplied-arg? limit)
                                      any/c
                                      (property/c length (<=/c limit))))])]))

; Note: these functions are not general-purpose -- it is in general,
; NOT safe to use them in arbitrary Rosette programs. At best,
; they might _increase_ the size of terms; at worst, they may produce
; incorrect results.
;
; We use these only in conjunction with yosys, where we have a situation
; where the assertion store is empty, and there is no mutation everywhere
; (it's all pure functional code), so I think this is okay.

(define (concretize term [error-on-failure #f])
  (define vars (@symbolics term))
  (cond
    [(empty? vars) term]
    [else
     (define model (@solve #t))
     (when (not (@sat? model))
       (error "can't find single concrete state"))
     (define term-concrete (@evaluate term (@complete-solution model vars)))
     (define must-be-same
       (@unsat?
        (@verify
         (@assert
          (@equal? term term-concrete)))))
     (cond
       [must-be-same term-concrete]
       [(not error-on-failure) term]
       [else (error 'concretize "failed to concretize term")])]))

(define-syntax (concretize-fields stx)
  (syntax-parse stx
    [(_ struct-id:id struct-elem:expr (field-name:id ...))
     #:with (getter-name ...) (for/list ([field-name (syntax->list #'(field-name ...))])
                                (format-id stx "~a-~a" (syntax-e #'struct-id) (syntax-e field-name)))
     #'(let ()
         (define struct-v struct-elem)
         (struct-copy
          struct-id
          struct-v
          [field-name (concretize (getter-name struct-v))] ...))]))

(define (concretize-all-fields state)
  (for/struct (v state)
    (concretize v)))

(define (all-values term #:limit [limit #f])
  (define vars (@symbolics term))
  (let rec ([acc '()]
            [neq-rest #t]
            [sofar 0])
    (cond
      [(and limit (>= sofar limit)) acc]
      [else
       (define model (@solve (@assert neq-rest)))
       (cond
         [(@unsat? model) acc]
         [(@unknown? model) acc] ; give up
         [else ; sat
          (define concrete (@evaluate term (@complete-solution model vars)))
          (rec (cons concrete acc)
               (@&& neq-rest (@not (@equal? term concrete)))
               (add1 sofar))])])))
