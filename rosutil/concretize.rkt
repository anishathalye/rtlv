#lang racket/base

(require racket/list racket/contract
         yosys
         (prefix-in @ rosette/safe)
         (for-syntax racket/base syntax/parse racket/syntax))

(provide
 (contract-out
  [concrete (-> any/c
                @solution?)]
  [concretize (->* (any/c)
                   (#:error-on-failure boolean?)
                   any)]
  [concretize-fields (-> yosys-module?
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

(define (concretize term #:error-on-failure [error-on-failure #f])
  (define-values (term-concrete ok) (concretize* term))
  (if (or ok (not error-on-failure))
      term-concrete
      (error 'concretize "failed to concretize term")))

(define (concrete term)
  (define-values (_ res) (concretize* term))
  res)

(define (concretize* term)
  (define vars (@symbolics term))
  (cond
    [(empty? vars) (values term (@unsat))]
    [else
     ;; we don't use the assertion store / assume the assertion store
     ;; is empty, so an empty model will do (solution to (solve #t)), but this
     ;; avoids the solver call. even if this assumption is wrong, this
     ;; will not result in unsoundness (we might just fail to concretize)
     (define model (@sat))
     (define term-concrete (@evaluate term (@complete-solution model vars)))
     (define res
       (@verify
        (@assert
         (@equal? term term-concrete))))
     (if (@unsat? res)
         (values term-concrete res)
         (values term res))]))

(define (concretize-fields state)
  (for/struct [(n v) state]
    (if ((field-filter) n)
        (concretize v)
        v)))

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
