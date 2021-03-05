#lang racket/base

(require racket/bool racket/list racket/format racket/contract racket/match racket/set racket/function
         syntax/parse/define
         yosys
         (prefix-in @ (combine-in rosette/safe rosutil))
         (for-syntax racket/base syntax/parse))

(provide
 (contract-out
  [with-invariants (->*
                    (dynamically-addressable? (-> dynamically-addressable? any))
                    (dynamically-addressable?)
                    dynamically-addressable?)]
  [verify-deterministic-start (->*
                               ((-> (and/c yosys-module? dynamically-addressable?))
                                #:invariant (-> (and/c yosys-module? dynamically-addressable?) any)
                                #:step (-> (and/c yosys-module? dynamically-addressable?) (and/c yosys-module? dynamically-addressable?))
                                #:reset symbol?
                                #:reset-active (or/c 'low 'high)
                                #:inputs (listof (or/c symbol? (cons/c symbol? wire-constant?)))
                                #:state-getters (listof (cons/c symbol? (-> (and/c yosys-module? dynamically-addressable?) any))))
                               (#:output-getters (listof (cons/c symbol? (-> (and/c yosys-module? dynamically-addressable?) any)))
                                #:hints (->* (symbol?) #:rest any/c any)
                                #:print-style (or/c 'full 'names 'none)
                                #:try-verify-after natural-number/c
                                #:limit (or/c #f natural-number/c)
                                #:debug (-> natural-number/c (and/c yosys-module? dynamically-addressable?) (or/c #f @solution?) (or/c #f (and/c yosys-module? dynamically-addressable?))))
                               (or/c #f natural-number/c))]))

(define wire-constant?
  (flat-named-contract
   'wire-constant?
   (lambda (v)
     (match v
       [#t #t]
       [#f #t]
       [(@bv _ _) #t]
       [else #f]))))

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
  (unless (@sat? model-example)
    (error 'with-invariants "invariant unsatisfiable"))
  (for/struct ((n v) state)
    (define v-example (@evaluate v model-example))
    ; note: v-example may still have symbolics in it (we didn't call complete-solution on the model).
    ; this is an optimization: if a field is not concrete, we can skip it
    ; without an expensive call to the solver
    (define must-be-same
      (and (@concrete? v-example)
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

(define-simple-macro (time body ...)
  (time* (thunk body ...)))

(define-simple-macro (define+time (x:id t:id) body ...)
  (define-values (x t) (time body ...)))

(define/contract (static-values statics state)
  (-> (listof (or/c symbol? (cons/c symbol? (listof natural-number/c))))
      yosys-module?
      (listof (or/c @constant? @term?)))
  (for/fold ([acc '()])
            ([i statics])
    (cond
      [(symbol? i) (cons (get-field state i) acc)]
      [else
       ; symbol (for vector), list of indices
       (define name (car i))
       (define indices (cdr i))
       (define field (get-field state name))
       (append
        (for/list ([idx indices])
          (vector-ref field idx))
        acc)])))

(define (verify-statics s0-with-inv step statics)
  (define s1 (step s0-with-inv))
  (define res
    (@solve (@assert (@not (@equal? (static-values statics s0-with-inv)
                                    (static-values statics s1))))))
  (@unsat? res))

; extended for Yosys types (i.e. vectors)
(define (only-depends-on* value symbolics)
  (if (not (vector? value))
      (@only-depends-on/unchecked value symbolics)
      (let ()
        (define any-failed
          (for/or ([v value])
            (define r (@only-depends-on/unchecked v symbolics))
            (if (@unsat? r) #f r)))
        (if (not any-failed)
            (@unsat)
            any-failed))))

; symbolic-constructor: returns fully symbolic module
; statics: captures static state in module (that can't change at all, e.g. due to dead code); untrusted
;
; returns #f if verifying deterministic start failed after hitting the limit on number of cycles
; otherwise returns the number of cycles it took to verify (a truthy value)
(define (verify-deterministic-start
         symbolic-constructor
         #:invariant invariant
         #:step step
         #:reset reset
         #:reset-active reset-active
         #:inputs inputs
         #:state-getters state-getters
         #:output-getters [output-getters '()]
         #:hints [hints (lambda _ #f)]
         #:print-style [print-style 'full]
         #:try-verify-after [try-verify-after 0]
         #:limit [limit #f]
         #:debug [debug (lambda _ #f)])
  (define s0-with-inv (with-invariants (symbolic-constructor) invariant))
  (define statics (or (hints 'statics) '()))
  (unless (verify-statics s0-with-inv step statics)
    (error 'verify-deterministic-start "failed to prove statics"))
  (define allowed-dependencies (list->weak-seteq (static-values statics s0-with-inv)))
  (define sn s0-with-inv)
  (define verified #f)
  (define+time (_ total-time)
    (for ([cycle (in-naturals)])
      (printf "cycle ~a~n" cycle)

      (define current-inputs
        (for/list ([i (cons reset inputs)]) ; append reset just in case it's not present
          (cond
            [(pair? i) i] ; pre-set value
            [(eq? i reset) (cons i (if (xor (zero? cycle) (eq? reset-active 'low)) #t #f))] ; special-case reset
            [else ; symbol
             (define s (@fresh-symbolic i (@type-of (get-field sn i))))
             (set-add! allowed-dependencies s)
             (cons i s)])))
      (set! sn (update-fields sn current-inputs))

      (define+time (any-hints hint-time)
        (define this-hint (hints 'general cycle sn))
        (when this-hint
          (for ([hint this-hint])
            (match hint
              ; without gc, allowed-dependencies can grow very big
              ;
              ; we could prune it by intersecting it with (@symbolics sn),
              ; but it seems like using a weak set and using gc is actually faster
              ['collect-garbage (collect-garbage)]
              [(cons 'abstract args)
               ; like overapproximate, but we are allowed to depend on the fresh value
               ; because we prove that the term we're replacing only depends on inputs
               ;(define allowed-deps-list (set->list allowed-dependencies))
               (define updates
                 (for/list ([i args])
                   (define v (get-field sn i))
                   (define ok (@unsat? (@only-depends-on/unchecked v allowed-dependencies)))
                   (cons i (if ok
                               (let ([v* (@fresh-symbolic i (@type-of v))])
                                 (set-add! allowed-dependencies v*)
                                 v*)
                               v))))
               (set! sn (update-fields sn updates))]
              [(cons 'overapproximate args)
               (define updates
                 (for/list ([i args])
                   (define v (get-field sn i))
                   (define v* (if (vector? v)
                                  (@fresh-memory-like i v)
                                  (@fresh-symbolic i (@type-of v))))
                   (cons i v*)))
               (set! sn (update-fields sn updates))]
              [(cons 'concretize args)
               (define updates
                 (for/list ([i args])
                   (define v (get-field sn i))
                   (cons i (@concretize v))))
               (set! sn (update-fields sn updates))])))
        (not (not this-hint)))
      (when any-hints
        (printf "  handled hints in ~ams~n" (~r hint-time #:precision 1)))

      (define+time (res state-analysis-time)
        (if (>= cycle try-verify-after)
            (let ()
              (define states (map (lambda (f) (cons (car f) ((cdr f) sn))) state-getters))
              (define any-failed
                (for/or ([name-value states])
                  (match-define (cons name value) name-value)
                  (define r (only-depends-on* value allowed-dependencies))
                  (if (@unsat? r) #f (list name value r))))
              (if (not any-failed)
                  (let ()
                    (displayln " --> unsat!")
                    (@unsat))
                  (let ()
                    (match-define (list name value r) any-failed)
                    (cond
                      [(@unknown? r)
                       (displayln "warning: determinism check returned (unknown), treating as SAT and continuing")]
                      ; can't be @unsat?, we checked for that earlier
                      [else
                       (displayln "  -> sat!")
                       (case print-style
                         [(names) (printf "  ~a~n" name)]
                         [(full) (printf "  ~a: ~v~n" name value)])])
                    r)))
            #f))

      (let ([sn* (debug cycle sn res)])
        (when sn*
          (set! sn sn*)))

      (when (@unsat? res)
        (set! verified cycle))
      (when (>= cycle try-verify-after)
        (printf "  analyzed state in ~ams~n" (~r state-analysis-time #:precision 1)))
      #:break (or verified (and limit (>= cycle limit)))

      (define+time (sn+1 step-time) (step sn))
      (printf "  stepped in ~ams~n" (~r step-time #:precision 1))

      ; check outputs in a similar style as states
      ; except here, outputs need to be okay at every step
      (define+time (any-outputs-failed output-analysis-time)
        (define outputs (map (lambda (f) (cons (car f) ((cdr f) sn+1))) output-getters))
        (for/or ([name-value outputs])
          (match-define (cons name value) name-value)
          (define r (@only-depends-on/unchecked value allowed-dependencies))
          (if (@unsat? r) #f (list name value r))))
      (when (not (empty? output-getters))
        (printf "  analyzed outputs in ~ams~n" (~r output-analysis-time #:precision 1)))
      (when any-outputs-failed
        (match-define (list name value r) any-outputs-failed)
        (case print-style
          [(names) (printf "  failed: output ~a not deterministic~n" name)]
          [(full) (printf "  failed: output ~a not deterministic: ~v~n" name value)]))
      #:break any-outputs-failed

      (set! sn sn+1)))

  (define t (~r (/ total-time 1000) #:precision 1))
  (if verified
      (printf "finished in ~as~n" t)
      (printf "failed to prove (took ~as)~n" t))
  verified)
