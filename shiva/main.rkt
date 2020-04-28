#lang racket

(require (prefix-in r: rosette/safe)
         (for-syntax syntax/parse))

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
(define (with-invariants struct-constructor symbolic-state invariant-fn)
  (define state symbolic-state)
  (define model-example
    (r:solve (r:assert (invariant-fn state))))
  (when (not (r:sat? model-example))
    (error 'with-invariants "invariant unsatisfiable"))
  (define state-example
    (r:evaluate
     state
     (r:complete-solution model-example (r:symbolics state))))
  (define symbolic-state-vector (struct->vector state))
  (define concrete-state-vector (struct->vector state-example))
  (define maybe-concretized-fields
    (for/list ([i (in-range 1 (vector-length symbolic-state-vector))])
      (define symbolic-field-value (vector-ref symbolic-state-vector i))
      (define concrete-field-value (vector-ref concrete-state-vector i))
      (define must-be-same
        (r:unsat?
         (r:solve
          (r:assert
           (r:and (invariant-fn state)
                  (r:not (r:equal? symbolic-field-value concrete-field-value)))))))
      (if
       must-be-same
       ; in this case, we know it must always be equal to the concrete value we found
       concrete-field-value
       ; but if not, keep the old symbolic value
       symbolic-field-value)))
  (apply struct-constructor maybe-concretized-fields))

(module+ test
  (require rackunit)

  (test-case "with-invariants"
    (r:struct mod (x y) #:transparent)
    (define (symbolic-mod)
      (r:define-symbolic* x y (r:bitvector 32))
      (mod x y))
    (define (invariant m)
      (r:equal? (mod-x m) (r:bv #x1337 32)))
    (define m
      (with-invariants mod (symbolic-mod) invariant))
    (check-equal? (mod-x m) (r:bv #x1337 32))
    (check-pred r:constant? (mod-y m))))

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
    (define name (first e1))
    (define-values (v1 v2) (values (second e1) (second e2)))
    (define diff (not (r:equal? v1 v2)))
    (when diff
      (if only-names
          (printf "    ~a~n" name)
          (printf "    ~a: ~a != ~a~n" name v1 v2)))
    #:break (and diff only-first)
    (void)))

(define (verify-statics s0-with-inv step statics)
  (define s1 (step s0-with-inv))
  (define res
    (r:solve (r:assert (r:not (r:equal? (statics s0-with-inv) (statics s1))))))
  (r:unsat? res))

; struct-constructor: constructor for module?
; symbolic-constructor: returns fully symbolic module?
; invariant: module? -> boolean?
; step: module? -> module?
; init-input-setter: module? -> module?, sets input for reset state (cycle 0)
; input-setter: module? -> module?, sets input for all other cycles
; state-getters: module? -> list of (list name getter)
; statics: module? -> any?, captures static state in module (that can't change at all, e.g. due to dead code); untrusted
; overapproximate: module?, integer? -> module? or #f, returns potential overapproximation at a particular cycle; trusted / part of TCB
; print-style: 'full, 'names, or 'none
; try-verify-after: don't invoke SMT solver until given step
; debug: integer?, module?, model? -> (void), called at every step with cycle, state, and model
(define (verify-deterministic-start
         struct-constructor
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
         #:debug [debug #f])
  (define tick (current-inexact-milliseconds))
  (define s0-with-inv (with-invariants struct-constructor (symbolic-constructor) invariant))
  (when (not (verify-statics s0-with-inv step statics))
    (error 'verify-deterministic-start "failed to prove statics"))
  (define s0 (init-input-setter s0-with-inv))
  (define sn s0)
  (define-values (ignored total-time)
    (time
     (for ([cycle (in-naturals)])
       (printf "cycle ~a~n" cycle)
       (define res
         (if (cycle . >= . try-verify-after)
             (let ()
               (define states (r:map (r:lambda (f) (r:list (r:first f) ((r:second f) sn))) state-getters))
               ; note: we get (symbolics sn) rather than just of states, so that we can get a fully concrete state if we want
               (define symvars (r:symbolics sn))
               (define complete-soln (r:complete-solution (r:solve #t) symvars))
               (cond
                 [(r:unknown? complete-soln)
                  ; this isn't a fatal error; maybe things could become easier to solve in a future cycle
                  ; but it is unlikely
                  (displayln "warning: solver timed out while trying to find a single concrete solution, which might be a performance bug; treating as SAT and continuing")
                  #f]
                 [(r:unsat? complete-soln)
                  (error "state has no concrete solution: bug in input?")]
                 [else
                  (define states-concrete (r:evaluate states complete-soln))
                  (define sn-concrete (r:evaluate sn complete-soln))
                  (define-values (model query-time)
                    (time (r:solve (r:assert (r:and
                                              (r:not (r:equal? states states-concrete))
                                              (r:equal? (statics sn) (statics sn-concrete)))))))
                  (printf "  smt query returned in ~ams~n" (~r query-time #:precision 1))
                  (when debug
                    (debug cycle sn model))
                  (cond
                    [(r:unknown? model)
                     (displayln "warning: determinism check returned (unknown), treating as SAT and continuing")]
                    [(r:unsat? model)
                     (displayln "  -> unsat!")]
                    [else
                     (displayln "  -> sat!")
                     (when (not (eq? print-style 'none))
                       (define states-concrete-2 (r:evaluate states (r:complete-solution model symvars)))
                       (show-differences states-concrete states-concrete-2 #t (eq? print-style 'names)))])
                  model]))
             #f))
       #:break (r:unsat? res)
       (define-values (sn+1 step-time) (time (step sn)))
       (printf "  stepped in ~ams~n" (~r step-time #:precision 1))
       (set! sn
             (let* ([with-inputs (input-setter sn+1)]
                    [overapproximation (and overapproximate (overapproximate with-inputs cycle))])
               (or overapproximation with-inputs))))))
  (printf "finished in ~as~n" (~r (/ total-time 1000) #:precision 1)))

(provide verify-deterministic-start)
