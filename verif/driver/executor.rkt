#lang racket/base

(require
 (prefix-in interp: "interpreter.rkt")
 "hint.rkt"
 racket/class racket/set racket/match racket/list racket/function
 (prefix-in yosys: yosys)
 (prefix-in @ (combine-in rosutil rosette/safe))
 syntax/parse/define)

(provide executor%
         (prefix-out executor: (struct-out state)))

(struct state
  (interpreter-state
   pc)
  #:transparent)

;; pushes vc, evaluates body with pc added, and then ensures that vc hasn't changed
(define (with-pc* pc expr-thunk)
  (when (not (@vc-true? (@vc)))
    (error 'with-pc "initial vc must be true, is ~v" (@vc)))
  (define res (@with-vc (begin (@assume pc) (expr-thunk))))
  (when (not (@normal? res))
    (println res)
    (error 'with-pc "bug: evaluation did not terminate normally (all paths infeasible)"))
  (when (not (eq? (@vc-asserts (@result-state res)) #t))
    (error 'with-pc "evaluation of expression produced assertions")) ; we could probably handle this by checking and then discharging the asserts
  (@result-value res))

(define-simple-macro (with-pc pc expr)
  (with-pc* pc (lambda () expr)))

(define (finished? st)
  (not (interp:state? (state-interpreter-state st))))

(define executor%
  (class object%
    (super-new)
    (init initial-state)
    (init-field hint-db)
    (init [precondition #t])
    (define free-variables (weak-seteq))
    (define completed '())
    (define working (list (state initial-state precondition)))
    (define waiting '())
    (define waiting-for-debug* #f)
    (define merge-hint #f)
    (define debug #f)

    (define/public (debug!)
      (set! debug #t))

    (define solver (lambda (hint) (@current-solver)))
    (define/public (set-solver! hint-q s)
      (set! solver
            (let ([old-solver solver])
              (lambda (hint)
                (if (hint-q hint)
                    s
                    (old-solver hint))))))

    (define (dprintf . args)
      (when debug (apply printf args)))

    (define (run-single st)
      (define st* (run-to-next-hypercall st))
      (cond
        [(finished? st*)
         ;; value
         (set! completed (cons st* completed))]
        [else
         ;; hypercall
         (handle-hypercall st*)]))

    (define (handle-hypercall st)
      (define call (interp:state-control (state-interpreter-state st)))
      (unless (list? call)
        (error 'handle-hypercall "hypercall must be a concrete list, not a term (~v), try concretizing more?" call))
      (match-define (list fun hint-name) call)
      (define hint (hash-ref hint-db hint-name))
      (case fun
        [(yield)
         (unless (fixpoint? hint)
           (error 'handle-hypercall "argument to yield must be a fixpoint hint"))
         (define g (interp:state-globals (state-interpreter-state st)))
         (define ckt (interp:globals-circuit g))
         (define metadata (interp:globals-meta g))
         (define ckt-step (yosys:meta-step metadata))
         (match-define (fixpoint setup abstr len step-hints) hint)
         (define fp
           (parameterize ([@current-solver (solver hint)])
             (with-pc (state-pc st)
               (compute-fixpoint
                ckt-step
                ckt
                setup
                (make-field-abstractor abstr)
                len
                step-hints))))
         ;; step (interpreter) once more to advance past the call
         (define st1 (with-pc (state-pc st) (interp:step (state-interpreter-state st))))
         ;; make one state for every point in fp, put back on working list
         (set! working (append (for/list ([ckt fp]) (state (update-state-circuit st1 ckt) (state-pc st))) working))
         (dprintf "info: yielded, now have ~a threads, ~a waiting~n" (length working) (length waiting))]
        [(hint)
         (parameterize ([@current-solver (solver hint)])
           (cond
             [(circuit-hint? hint)
              (let* ([ckt (interp:globals-circuit (interp:state-globals (state-interpreter-state st)))]
                     [ckt* (with-pc (state-pc st) (apply-circuit-hint hint ckt))]
                     [st* (update-state-circuit (state-interpreter-state st) ckt*)])
                ;; step once more to advance past the call, put back on working list
                (set! working (cons (state (with-pc (state-pc st) (interp:step st*)) (state-pc st)) working)))]
             [(case-split? hint)
              ;; split interpreter N ways, augmenting path condition with splits
              ;;
              ;; if exhaustive, check that the split is indeed an
              ;; exhaustive split (given current pc)
              ;;
              ;; if not exhaustive, generate another split that's the
              ;; negation of all the splits and add it (but only if it's satisfiable)
              (define splits (do-case-split st hint))
              ;; step once more to advance past the call
              (set! working
                    (append
                     (map (lambda (st) (state (with-pc (state-pc st) (interp:step (state-interpreter-state st))) (state-pc st))) splits)
                     working))]
             [(merge? hint)
              ;; step once more to advance past the call, put on merge list
              (set! waiting (cons (state (with-pc (state-pc st) (interp:step (state-interpreter-state st))) (state-pc st)) waiting))
              (set! waiting-for-debug* #f)
              (unless merge-hint
                (set! merge-hint hint))]
             [(debug*? hint)
              ;; step once more to advance past the call, put on waiting list
              (set! waiting (cons (state (with-pc (state-pc st) (interp:step (state-interpreter-state st))) (state-pc st)) waiting))
              (unless waiting-for-debug*
                (set! waiting-for-debug* (debug*-fn hint)))]
             [(debug? hint)
              ;; pass interpreter state to debug function
              ((debug-fn hint) st)
              ;; step once more to advance past the call, put back on working list
              (set! working (cons (state (with-pc (state-pc st) (interp:step (state-interpreter-state st))) (state-pc st)) working))]
             [else (error 'handle-hypercall "unimplemented hint: ~a" hint)]))]))

    (define (apply-circuit-hint hint ckt)
      (match hint
        [(concretize field-filter)
         (parameterize ([yosys:field-filter field-filter])
           (@concretize-fields ckt))]
        [(overapproximate field-filter)
         ((make-field-abstractor (overapproximate-field-filter hint)) ckt)]))

    (define ((make-field-abstractor field-filter) st)
      (define ff (yosys:to-field-filter field-filter))
      (yosys:for/struct
       [(name value) st]
       (if (ff name)
           ;; replace it
           (if (vector? value)
               (let ([v (@fresh-memory-like/unchecked name value)])
                 (for ([el (in-vector v)])
                   (set-add! free-variables el))
                 v)
               (let ([v (@fresh-symbolic name (@type-of value))])
                 (set-add! free-variables v)
                 v))
           ;; keep it as-is
           value)))

    (define (do-debug*)
      (waiting-for-debug* waiting)
      (set! working waiting)
      (set! waiting '())
      (set! waiting-for-debug* #f))

    (define (do-case-split st hint)
      (define pc (state-pc st))
      (match-define (case-split splits* exhaustive) hint)
      (define splits
        (if exhaustive
            splits*
            (cons (@not (@foldl @|| #f splits*)) splits*)))
      ;; prune unsat splits
      (define pruned-splits
        (filter (lambda (p) (not (@unsat? (@solve (@assert (@&& pc p)))))) splits))
      ;; verify that the current pc implies that at least one of the splits must hold
      ;;
      ;; if we have splits p1, p2, ..., pn, we prove _here_ that
      ;; pc => (p1 \/ p2 \/ ... \/ pn), and we create n threads
      ;; with pcs (pc /\ p1), (pc /\ p2), ..., (pc /\ pn)
      (define any-split (foldl @|| #f pruned-splits))
      (when (not (@unsat? (@verify (@assert (@implies pc any-split)))))
        (error 'do-case-split "failed to prove exhaustiveness"))
      (define res
        (for/list ([p pruned-splits])
          (state (state-interpreter-state st) (@&& pc p))))
      (dprintf "info: case split ~a ways~n" (length res))
      res)

    (define (merge-states)
      ;; first, partition by path condition (we never merge across different PCs)
      (define by-pc (group-by state-pc waiting))
      (set! working (apply append (map merge-states-for-pc by-pc)))
      (dprintf "info: merged, reduced from ~v interp:states to ~v interp:states~n" (length waiting) (length working))
      (set! waiting '())
      (set! merge-hint #f))

    (define (merge-states-for-pc sts)
      ;; right now, we only handle the case where the rest of the
      ;; interpreter state is identical between all forks; in the future, we
      ;; could partition by interpreter state and support multiple, but I don't
      ;; think this is necessary right now
      (define template-state (first sts))
      (define template-interpreter (state-interpreter-state template-state))
      (for ([st (rest sts)])
        (define ist (state-interpreter-state st))
        ;; note: we don't check globals, because we're expecting the
        ;; circuit to differ, and we're not expecting the environment or meta
        ;; to differ (it never changes)
        (unless (or (and (interp:finished? ist) (interp:finished? template-interpreter)) ; XXX why do we need this check?
                    (and (@=? (interp:state-control ist) (interp:state-control template-interpreter))
                         (@=? (interp:state-environment ist) (interp:state-environment template-interpreter))
                         (@=? (interp:state-continuation ist) (interp:state-continuation template-interpreter))))
          (error 'merge-states "interp:states differ in aspects other than circuit")))
      (define ckts
        (shrink-by-key
         (map
          (lambda (st)
            (define s (state-interpreter-state st))
            (if (interp:finished? s) (interp:finished-circuit s) (interp:globals-circuit (interp:state-globals s))))
          sts)
         (merge-key merge-hint)))
      (for/list ([ckt ckts])
        (state (update-state-circuit template-interpreter ckt) (state-pc template-state))))

    (define (compute-fixpoint step s0 setup-cycles abstract-fn cycle-length step-hints)
      (define (step* ckt)
        ;; step, but applying step-hints after every step
        (for/fold ([ckt (step ckt)])
                  ([hint (if (list? step-hints) step-hints (list step-hints))])
          (apply-circuit-hint hint ckt)))
      (define rev-prefix (rev-step-n step* s0 setup-cycles))
      (define sn (first rev-prefix))
      (define sn-abs (abstract-fn sn))
      (define rev-abs-stepped (rev-step-n step* sn-abs cycle-length))
      ;; make sure it is indeed a fixpoint, only need to check this last bit since others were computed by step
      (define next-step (step* (first rev-abs-stepped)))
      (unless (field-subset? free-variables next-step sn-abs)
        (dprintf "next step: ~v~n does not loop back to~nabstracted: ~v~ndiff: ~a~n" next-step sn-abs (yosys:show-diff next-step sn-abs))
        (error 'compute-fixpoint "Did not find a fixpoint"))
      (reverse (append rev-abs-stepped (rest rev-prefix))))

    (define (shrink-by-key s key)
      (define groups (group-by key s))
      (dprintf "info: merging, have ~a interp:states grouped into ~a partitions~n" (length s) (length groups))
      (apply append (map shrink-set groups)))

    ;; aims to be sound, can't be "complete" (what is complete?)
    (define (shrink-set s)
      (dprintf "info: merging ~a interp:states~n" (length s))
      (let loop ([pending (reverse s)]
                 [keep '()])
        (if (empty? pending)
            (begin
              (dprintf "info: merged into ~a interp:states~n" (length keep))
              keep)
            (let ()
              (define p (first pending))
              (define pp (rest pending))
              (define represented
                (for/or ([k keep])
                  (field-subset? free-variables p k)))
              (if represented
                  (loop pp keep)
                  ;; need to add
                  (loop pp (cons p keep)))))))

    (define (update-state-circuit st ckt)
      (if (interp:state? st)
          (interp:state
           (interp:state-control st)
           (interp:state-environment st)
           (interp:update-circuit (interp:state-globals st) ckt)
           (interp:state-continuation st))
          (interp:finished (interp:finished-value st) ckt)))

    (define (run-to-next-hypercall st)
      (if (not (finished? st))
          (if (in-hypercall? (state-interpreter-state st))
              st ; return as-is
              (run-to-next-hypercall (state (with-pc (state-pc st) (interp:step (state-interpreter-state st))) (state-pc st))))
          ;; value
          st))

    (define (in-hypercall? st)
      (interp:uninterpreted? (interp:state-control st)))

    (define/public (run!)
      (cond
        [(and (empty? working) (empty? waiting))
         ;; we are completed, return values
         completed]
        [(not (empty? working))
         ;; more "single threads" to execute
         (match-define (cons h t) working)
         (set! working t)
         (run-single h)
         (run!)]
        [else
         ;; merge
         (if waiting-for-debug*
             (do-debug*)
             (merge-states))
         (run!)]))))

(define (@=? a b)
  (equal? (@equal? a b) #t))

;; this stuff is only safe to use on Yosys modules (so field types are
;; bitvector, bool, and vector of bitvector)
;;
;; these functions are aiming to be sound but not "complete", they may
;; say that something is not a subset when it actually is. doing
;; complete analysis seems expensive (calls to solver)

(define (term-subset? free-symbolics small big)
  (define types (map @type-of small))
  (define temp (map (curry @fresh-symbolic "__temp") types))
  (define q
    (@forall
     temp
     (@implies
      (@exists (set->list free-symbolics) (@equal? small temp))
      (@exists (set->list free-symbolics) (@equal? big temp)))))
  (define res (@verify (@assert q)))
  (cond
    [(@unsat? res) #t]
    [else #f]))

(define (field-subset? free-symbolics small big)
  (let/cc return
    (when (eq? small big)
      (return #t))
    ;; get all the fields
    (define all-fields (yosys:fields small))
    ;; filter out the ones that are equal? to each other
    (define-values (small-fields-filtered big-fields-filtered skipped-fields)
      (for/fold ([s '()]
                 [b '()]
                 [skipped-fields '()])
                ([field-name all-fields])
        (define field-s (yosys:get-field small field-name))
        (define field-b (yosys:get-field big field-name))
        (cond
          [(equal? field-s field-b) (values s b (cons field-name skipped-fields))]
          [(and (@concrete? field-s) (@concrete? field-b) (not (equal? s b))) (return #f)]
          [else (values (cons field-s s) (cons field-b b) skipped-fields)])))
    ;; bail out if any captured fields share symbolics that are free
    ;; with any non-captured fields: this case is quite complicated
    ;; to handle, and I don't know if we need to figure out how to handle it,
    ;; it may just not come up
    (cond
      ;; check shared symbolics
      [(let* ([small-symbolics (apply set-union (seteq) (map (compose1 list->seteq @symbolics) small-fields-filtered))]
              [big-symbolics (apply set-union (seteq) (map (compose1 list->seteq @symbolics) big-fields-filtered))]
              [fields-symbolics (set-union small-symbolics big-symbolics)])
         (for/or ([obj (list small big)])
           (for/or ([field-name skipped-fields])
             (not (set-empty? (set-intersect (list->seteq (@symbolics (yosys:get-field obj field-name))) fields-symbolics))))))
       #f]
      ;; check if any fields left are a vector?, fail if so
      [(for/or ([s small-fields-filtered]
                [b big-fields-filtered])
         (or (@vector? s) (@vector? b)))
       #f]
      [else
       (term-subset? free-symbolics small-fields-filtered big-fields-filtered)])))

(module+ test
  (require rackunit)

  (struct mod (a b c) #:transparent
    #:methods yosys:gen:yosys-module []
    #:methods yosys:gen:dynamically-addressable
    [(define (fields p) '(a b c))
     (define (get-field p s)
       (case s
         [(a) (mod-a p)]
         [(b) (mod-b p)]
         [(c) (mod-c p)]))
     (define (field-type p s)
       (error "not implemented"))
     (define (map-fields p f)
       (error "not implemented"))])

  (test-case "field-subset? input-sensitive"
    (@define-symbolic* x y (@bitvector 32))
    (define s1 (mod (@bv 1 8) x y))
    (define s2 (mod (@bv 1 8) y x))
    (check-false (field-subset? (seteq) s1 s2))
    (check-true (field-subset? (seteq x y) s1 s2)))

  (test-case "field-subset? abstract"
    (@define-symbolic* x y (@bitvector 32))
    (check-true (field-subset? (seteq x y) (mod #f #f x) (mod #f #f y)))
    (check-true (field-subset? (seteq y) (mod #f #f x) (mod #f #f y)))
    (check-false (field-subset? (seteq x) (mod #f #f x) (mod #f #f y))))

  (test-case "field-subset? basic math"
    (@define-symbolic* x (@bitvector 32))
    (check-true (field-subset? (seteq x) (mod #f #f x) (mod #f #f (@bvadd x (@bv 1 32)))))
    (check-true (field-subset? (seteq x) (mod #f #f (@bvadd x (@bv 1 32))) (mod #f #f x)))
    (check-false (field-subset? (seteq x) (mod #f #f x) (mod #f #f (@bvmul x (@bv 2 32)))))
    (check-true (field-subset? (seteq x) (mod #f #f (@bvmul x (@bv 2 32))) (mod #f #f x))))

  (test-case "field-subset? entanglement"
    (@define-symbolic* x y (@bitvector 32))
    (check-false (field-subset? (seteq x y) (mod #f x y) (mod #f x x)))
    ;; the following doesn't work because we skip the equal field, and
    ;; then we bail out during the "check free symbolics" check; if this kind
    ;; of case comes up in real examples, we can figure out how to handle it
    ;; more elegantly
    #;(check-true (field-subset? (seteq x y) (mod #f x x) (mod #f x y)))
    (check-false (field-subset? (seteq x y) (mod #f x x) (mod #f x (@bvadd x (@bv 1 32)))))
    (check-true (field-subset? (seteq x y) (mod #f x x) (mod #f y (@bvadd x (@bv 1 32)))))))

(define (rev-step-n step s0 n)
  (let loop ([s s0]
             [n n])
    (if (zero? n)
        (list s)
        (let* ([ss (loop s (sub1 n))]
               [sn-1 (first ss)])
          (cons (step sn-1) ss)))))
