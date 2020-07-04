#lang racket/base

(require racket/list racket/format racket/contract
         yosys
         (prefix-in @ rosette/safe)
         (for-syntax racket/base syntax/parse))

(provide
 (contract-out
  [with-invariants (->*
                    (yosys-module? (-> yosys-module? any))
                    (yosys-module?)
                    yosys-module?)]
  [verify-deterministic-start (->*
                               ((-> yosys-module?)
                                #:invariant (-> yosys-module? any)
                                #:step (-> yosys-module? yosys-module?)
                                #:init-input-setter (-> yosys-module? any)
                                #:input-setter (-> yosys-module? any)
                                #:state-getters (listof (cons/c symbol? (-> yosys-module? any))))
                               (#:statics (-> yosys-module? any)
                                #:overapproximate (or/c #f (-> yosys-module? natural-number/c (or/c #f yosys-module?)))
                                #:print-style (or/c 'full 'names 'none)
                                #:try-verify-after natural-number/c
                                #:limit (or/c #f natural-number/c)
                                #:debug (or/c #f (-> natural-number/c yosys-module? @solution? any)))
                               (or/c #f natural-number/c))]))

; state should be a new state constructed with new-symbolic-{type}
;
; with-invariants will return that state with the constant fields, as
; indicated by the invariant, to their constant values rather than being
; unconstrained (as they are when returned by new-symbolic-{type})
;
; this does a best-effort job at figuring out constant fields in the state
;
; it won't work, for example, for a large memory where a part of it
; is a constant, but the rest is undefined at initialization
(define (with-invariants symbolic-state invariant-fn [fallback #f])
  (define state symbolic-state)
  (define model-example
    (@solve (@assert (invariant-fn state))))
  (when (not (@sat? model-example))
    (error 'with-invariants "invariant unsatisfiable"))
  (for/struct ((n v) state)
    (define v-example (@evaluate v model-example))
    ; note: v-example may still have symbolics in it (we didn't call complete-solution on the model).
    ; this is an optimization: if a field is not concrete, we can skip it
    ; without an expensive call to the solver
    (define must-be-same
      (and (empty? (@symbolics v-example))
           (@unsat?
            (@solve
             (@assert
              (@and (invariant-fn state)
                    (@not (@equal? v v-example))))))))
    (cond
      [must-be-same v-example]
      [fallback (get-field fallback n)]
      [else v])))

(define (time* thunk)
  (define start (current-inexact-milliseconds))
  (define value (thunk))
  (define end (current-inexact-milliseconds))
  (values value (- end start)))

(define-syntax (time stx)
  (syntax-parse stx
    [(_ e:expr ...)
     #'(time* (lambda () e ...))]))

(define (show-differences s1 s2 [only-first #f] [only-names #f])
  (for ([e1 s1]
        [e2 s2])
    (define name (car e1))
    (define-values (v1 v2) (values (cdr e1) (cdr e2)))
    (define diff (not (@equal? v1 v2)))
    (when diff
      (if only-names
          (printf "    ~a~n" name)
          (printf "    ~a: ~a != ~a~n" name v1 v2)))
    #:break (and diff only-first)
    (void)))

(define (verify-statics s0-with-inv step statics)
  (define s1 (step s0-with-inv))
  (define res
    (@solve (@assert (@not (@equal? (statics s0-with-inv) (statics s1))))))
  (@unsat? res))

; symbolic-constructor: returns fully symbolic module
; init-input-setter: sets input for reset state (cycle 0)
; input-setter: sets input for all other cycles
; statics: captures static state in module (that can't change at all, e.g. due to dead code); untrusted
; overapproximate: returns potential overapproximation at a particular cycle; trusted / part of TCB
;
; returns #f if verifying deterministic start failed after hitting the limit on number of cycles
; otherwise returns the number of cycles it took to verify (a truthy value)
(define (verify-deterministic-start
         symbolic-constructor
         #:invariant invariant
         #:step step
         #:init-input-setter init-input-setter
         #:input-setter input-setter
         #:state-getters state-getters
         #:statics [statics (lambda (s) #f)]
         #:overapproximate [overapproximate #f]
         #:print-style [print-style 'full]
         #:try-verify-after [try-verify-after 0]
         #:limit [limit #f]
         #:debug [debug #f])
  (define s0-with-inv (with-invariants (symbolic-constructor) invariant))
  (when (not (verify-statics s0-with-inv step statics))
    (error 'verify-deterministic-start "failed to prove statics"))
  (define s0 (init-input-setter s0-with-inv))
  (define sn s0)
  (define verified #f)
  (define-values (ignored total-time)
    (time
     (for ([cycle (in-naturals)])
       (printf "cycle ~a~n" cycle)
       (define res
         (if (cycle . >= . try-verify-after)
             (let ()
               (define states (map (lambda (f) (cons (car f) ((cdr f) sn))) state-getters))
               ; note: we get (symbolics sn) rather than just of states, so that we can get a fully concrete state if we want
               (define symvars (@symbolics sn))
               (define complete-soln (@complete-solution (@solve #t) symvars))
               (cond
                 [(@unknown? complete-soln)
                  ; this isn't a fatal error; maybe things could become easier to solve in a future cycle
                  ; but it is unlikely
                  (displayln "warning: solver timed out while trying to find a single concrete solution, which might be a performance bug; treating as SAT and continuing")
                  #f]
                 [(@unsat? complete-soln)
                  (error "state has no concrete solution: bug in input?")]
                 [else
                  (define states-concrete (@evaluate states complete-soln))
                  (define sn-concrete (@evaluate sn complete-soln))
                  (define-values (model query-time)
                    (time (@solve (@assert (@and
                                            (@not (@equal? states states-concrete))
                                            (@equal? (statics sn) (statics sn-concrete)))))))
                  (printf "  smt query returned in ~ams~n" (~r query-time #:precision 1))
                  (when debug
                    (debug cycle sn model))
                  (cond
                    [(@unknown? model)
                     (displayln "warning: determinism check returned (unknown), treating as SAT and continuing")]
                    [(@unsat? model)
                     (displayln "  -> unsat!")]
                    [else
                     (displayln "  -> sat!")
                     (when (not (eq? print-style 'none))
                       (define states-concrete-2 (@evaluate states (@complete-solution model symvars)))
                       (show-differences states-concrete states-concrete-2 #t (eq? print-style 'names)))])
                  model]))
             #f))
       (when (@unsat? res)
         (set! verified cycle))
       #:break (or verified (and limit (>= cycle limit)))
       (define-values (sn+1 step-time) (time (step sn)))
       (printf "  stepped in ~ams~n" (~r step-time #:precision 1))
       (set! sn
             (let* ([with-inputs (input-setter sn+1)]
                    [overapproximation (and overapproximate (overapproximate with-inputs cycle))])
               (or overapproximation with-inputs))))))
  (define t (~r (/ total-time 1000) #:precision 1))
  (if verified
      (printf "finished in ~as~n" t)
      (printf "failed to prove in ~a cycles (took ~as)~n" limit t))
  verified)
