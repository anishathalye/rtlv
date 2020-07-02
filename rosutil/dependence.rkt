#lang racket/base

(require "concretize.rkt"
         racket/contract racket/set racket/list
         (prefix-in @ rosette/safe))

(provide
 (contract-out
  [only-depends-on (->
                    value-of-solvable-type?
                    (listof @constant?)
                    boolean?)]))

(define value-of-solvable-type?
  (flat-named-contract
   'value-of-solvable-type?
   (lambda (v) (@solvable? (@type-of v)))))

; is value fully determined by an assignment of concrete values
; to the given symbolics?
(define (only-depends-on value constants)
  (define value-symbolics (apply seteq (@symbolics value)))
  (define allowed-dependencies (apply seteq constants))
  (cond
    [(subset? value-symbolics allowed-dependencies)
     ; fast path
     #t]
    [(empty? constants)
     ; fast-ish path when we're trying to show that something
     ; is concrete (no dependence on any constants)
     (concrete? value)]
    [else
     ; need to invoke solver
     (define other-symbolics (set-subtract value-symbolics allowed-dependencies))
     ; try to show that value doesn't depend on other symbolics
     (@define-symbolic* fresh (@type-of value))
     (define res
       (@verify
        (@assert
         (@exists (list fresh)
                  (@forall (set->list other-symbolics)
                           (@equal? value fresh))))))
     (@unsat? res)]))
