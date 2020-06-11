#lang rosette/safe

(require (for-syntax syntax/parse
                     racket/syntax)
         syntax/parse/define)

; we need to be careful of what we import for runtime, because
; whatever we use needs to be compatible with Rosette
(require
 "memoize.rkt"
 "parameters.rkt"
 rosutil
 (prefix-in racket: racket))

(begin-for-syntax
  (define-splicing-syntax-class yosys-type
    #:attributes (show ctor zero-ctor)
    (pattern (~datum Bool)
             #:with show #'show-boolean
             #:attr ctor (lambda (name-stx) #`(fresh-symbolic #,name-stx boolean?))
             #:with zero-ctor #'#f)
    (pattern ((~datum _) (~datum BitVec) num:nat)
             #:with show #'show-bitvector
             #:attr ctor (lambda (name-stx) #`(fresh-symbolic #,name-stx (bitvector num)))
             #:with zero-ctor #'(bv 0 num))
    (pattern ((~datum Array) ((~datum _) (~datum BitVec) depth:nat) ((~datum _) (~datum BitVec) width:nat))
             #:with show #'show-array
             #:attr ctor (lambda (name-stx)
                           #`(if (array-representation-vector)
                                 ; vector representation
                                 (list->vector
                                  (build-list (expt 2 depth)
                                              (lambda (_)
                                                (fresh-symbolic #,name-stx (bitvector width)))))
                                 ; UF representation
                                 (fresh-symbolic #,name-stx (~> (bitvector depth) (bitvector width)))))
             #:with zero-ctor #'(if (array-representation-vector)
                                    ; vector representation
                                    (list->vector
                                     (replicate (expt 2 depth) (bv 0 width)))
                                    ; UF representation
                                    (lambda (addr) (bv 0 width)))))

  (define-splicing-syntax-class yosys-member
    #:attributes (external-name name show ctor zero-ctor)
    (pattern (~seq (name:id type:yosys-type) external-name:id)
             #:with show (attribute type.show)
             #:with ctor ((attribute type.ctor) #'external-name)
             #:with zero-ctor (attribute type.zero-ctor)))

  (define-syntax-class yosys-initial-state
    #:attributes (name ctor zero-ctor)
    (pattern (name:id (~datum Bool))
             #:with ctor #'(fresh-symbolic name boolean?)
             #:with zero-ctor #'#f)))

(define-syntax (declare-datatype stx)
  (syntax-parse stx
    [(_ datatype-name:id ((_
                           init:yosys-initial-state
                           member:yosys-member ...)))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'datatype-name)
     #:with new-symbolic-name (format-id stx "new-symbolic-~a" #'datatype-name)
     #:with new-zeroed-name (format-id stx "new-zeroed-~a" #'datatype-name)
     #:with init-getter (format-id stx "~a-~a" #'datatype-name #'init.name)
     #:with (getter ...) (for/list ([external-name (syntax->list #'(member.external-name ...))])
                           (format-id stx "~a-~a" #'datatype-name external-name))
     #:with /... (quote-syntax ...)
     #`(begin
         (struct datatype-name (init.name member.external-name ...)
           #:methods gen:custom-write
           [(define (write-proc x port mode) (show x port mode))]
           #:transparent)
         (define (show x port mode)
           (cond
             [(boolean? mode)
              ; write or display
              (fprintf port "#(struct:~a " 'datatype-name)
              (show-boolean (init-getter x) port mode)
              (begin
                (fprintf port " ")
                (member.show (getter x) port mode)) ...
              (fprintf port ")")]
             [else
              ; print something more human-readable
              (fprintf port "~a {~n" 'datatype-name)
              (begin
                (fprintf port "  ~a:" 'member.external-name)
                (member.show (getter x) port mode)
                (fprintf port "\n")) ...
              (fprintf port "}")]))
         (provide datatype-name)
         ; like struct-copy, but uses the internal names instead of external names
         (begin-for-syntax
           (define name-assoc
             (make-hash '((member.name . member.external-name) ...))))
         (define-syntax (internal-copy-name stx)
           (syntax-parse stx
             [(_ type state [internal-name value] /...)
              #`(racket:struct-copy
                 type
                 state
                 #,@(for/list ([i (syntax->list #'(internal-name /...))]
                               [v (syntax->list #'(value /...))])
                      #`(#,(datum->syntax #'datatype-name (hash-ref name-assoc (syntax-e i))) #,v)))
              ]))
         (define init.name init-getter)
         (define member.name getter) ... ; for internal use only, not exported
         (define member.external-name getter) ...
         (provide getter) ...
         (define (new-symbolic-name)
           (datatype-name init.ctor member.ctor ...))
         (provide new-symbolic-name)
         (define (new-zeroed-name)
           (datatype-name init.zero-ctor member.zero-ctor ...))
         (provide new-zeroed-name))]))
(provide declare-datatype)

(define define-fun-hooks '())

(define (add-define-fun-hook hook)
  (set! define-fun-hooks (cons hook define-fun-hooks)))

(define (trigger-hooks name fn)
  (racket:for ([hook define-fun-hooks])
              (hook name fn))
  (set! define-fun-hooks '()))

(define-syntax (define-fun stx)
  (syntax-parse stx
    ; regular case
    [(_ name:id ((input:id input-type)) return-type body:expr)
     #'(begin
         (define/memoize1 (name input)
           body)
         (provide name)
         (trigger-hooks 'name name))]
    ; transition function: treated specially
    ; case 1: when module is purely combinatorial
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        (~datum true))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'type)
     #'(begin
         (define (name state)
           (new-memoization-context
            (internal-copy-name type state)))
         (provide name)
         (trigger-hooks 'name name))]
    ; case 2: when state has a single field
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        ((~datum =) e (field:id (~datum next_state))))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'type)
     #`(begin
         (define (name state)
           (new-memoization-context
            (internal-copy-name type state [field e])))
         (provide name)
         (trigger-hooks 'name name))]
    ; case 3: when state has multiple fields
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        ((~datum and) ((~datum =) e (field:id (~datum next_state))) ...))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'type)
     #`(begin
         (define (name state)
           (new-memoization-context
            (internal-copy-name type state [field e] ...)))
         (provide name)
         (trigger-hooks 'name name))]))
(provide define-fun)

(define (extractor i j)
  (lambda (x) (extract i j x)))

(define-simple-macro (_ (~datum extract) i:expr j:expr)
  (extractor i j))
(provide _)

(define-simple-macro (ite c t f)
  (if c t f))
(provide ite)

(define (distinct x y)
  (not (bveq x y)))
(provide distinct)

(define (select a i)
  (if (array-representation-vector)
      ; vector representation
      (let ([symbolic-index (not (concrete-head? i))]
            [thresh (overapproximate-symbolic-load-threshold)])
        (if (and symbolic-index thresh (>= (vector-length a) thresh))
            ; overapproximate, return fresh symbolic value
            (fresh-symbolic overapproximated-value (type-of (vector-ref a 0)))
            ; do the indexing into the vector
            (vector-ref a (bitvector->natural i))))
      ; UF representation
      (a i)))
(provide select)

(define (vector-update vec pos v)
  (define symbolic-index (not (concrete-head? pos)))
  (define thresh (overapproximate-symbolic-store-threshold))
  (if (and symbolic-index thresh (>= (vector-length vec) thresh))
      (let ([type (type-of (vector-ref vec 0))])
        (list->vector
         (build-list (vector-length vec)
                     (lambda (_) (fresh-symbolic overapproximation type)))))
      ; XXX this seems inefficient
      (let ([vec-copy (apply vector (vector->list vec))])
        (vector-set! vec-copy pos v)
        vec-copy)))

(define (store a i v)
  (if (array-representation-vector)
      ; vector representation
      (vector-update a (bitvector->natural i) v)
      ; UF representation
      (lambda (i*) (if (bveq i i*) v (a i*)))))
(provide store)

; we could implement this with `equal?`, but that is slow. Yosys uses `=` mostly for
; bitvectors, and only in a few cases for booleans. The boolean cases are:
;
; - in the invariant function, when comparing a boolean with the literal 'true' or 'false'
; - in the transition function (this is a macro anyways, that treats the '=' specially)
(define-syntax (= stx)
  (syntax-parse stx
    [(_ x:expr (~datum true))
     #'(<=> x true)]
    [(_ x:expr (~datum false))
     #'(<=> x false)]
    [(_ x:expr y:expr)
     #'(bveq x y)]))
(provide =)

(provide (rename-out [! not]
                     [&& and]
                     [|| or]))

(define (<=>* . args)
  (foldl <=> #t args))

; to match SMTLIB's xor, which can take multiple arguments
(define-syntax (varargs-xor stx)
  (syntax-parse stx
    [(_ (~seq a0 a1) ...) #'(! (<=>* (~@ a0 a1) ...))]
    [(_ a (~seq b0 b1) ...) #'(<=>* a (~@ b0 b1) ...)]))
(provide (rename-out [varargs-xor xor]))

(module+ test
  (require rackunit)
  (test-case "xor"
    (define-symbolic* a b c d e f boolean?)
    (define (reference-xor x y)
      (! (<=> x y)))
    (define (reference-varargs-xor . args)
      (foldl reference-xor #f args))
    (check-pred unsat? (verify (assert (equal? (reference-varargs-xor a) (varargs-xor a)))))
    (check-pred unsat? (verify (assert (equal? (reference-varargs-xor a b) (varargs-xor a b)))))
    (check-pred unsat? (verify (assert (equal? (reference-varargs-xor a b c) (varargs-xor a b c)))))
    (check-pred unsat? (verify (assert (equal? (reference-varargs-xor a b c d) (varargs-xor a b c d)))))
    (check-pred unsat? (verify (assert (equal? (reference-varargs-xor a b c d e) (varargs-xor a b c d e)))))))

; this appears at the top of the extraction, so we can put global
; top-level definitions here
(define-syntax (yosys-smt2-stdt stx)
  (syntax-parse stx
    [(_)
     #:with inputs (format-id stx "inputs")
     #:with outputs (format-id stx "outputs")
     #:with registers (format-id stx "registers")
     #:with memories (format-id stx "memories")
     #'(begin
         (define inputs '())
         (provide inputs)
         (define outputs '())
         (provide outputs)
         (define registers '())
         (provide registers)
         (define memories '())
         (provide memories))]))
(provide yosys-smt2-stdt)

; no interpretation yet
(define-simple-macro (yosys-smt2-module name:id)
  (void))
(provide yosys-smt2-module)

; no interpretation yet
(define-simple-macro (yosys-smt2-topmod name:id)
  (void))
(provide yosys-smt2-topmod)

; no interpretation yet
(define-simple-macro (yosys-smt2-clock name:id 'edge:id)
  (void))
(provide yosys-smt2-clock)

(define-simple-macro (make-appender lst)
  (lambda (name fn) (set! lst (cons (list name fn) lst))))

(define-syntax (yosys-smt2-input stx)
  (syntax-parse stx
    [(_ name:id width:nat)
     #:with inputs (format-id stx "inputs")
     #'(add-define-fun-hook (make-appender inputs))]))
(provide yosys-smt2-input)

(define-syntax (yosys-smt2-output stx)
  (syntax-parse stx
    [(_ name:id width:nat)
     #:with outputs (format-id stx "outputs")
     #'(add-define-fun-hook (make-appender outputs))]))
(provide yosys-smt2-output)

(define-syntax (yosys-smt2-register stx)
  (syntax-parse stx
    [(_ name:id width:nat)
     #:with registers (format-id stx "registers")
     #'(add-define-fun-hook (make-appender registers))]))
(provide yosys-smt2-register)

(define-syntax (yosys-smt2-memory stx)
  (syntax-parse stx
    [(_ name:id bits:nat width:nat read-ports:nat write-ports:nat 'sync:id)
     #:with memories (format-id stx "memories")
     #'(add-define-fun-hook (make-appender memories))]))
(provide yosys-smt2-memory)

(define (show-recur x port mode)
  (case mode
    [(#t) (write x port)]
    [(#f) (display x port)]
    [else (fprintf port " ") (print x port mode)]))

(define (show-boolean x port mode)
  (show-recur x port mode))

(define (show-bitvector x port mode)
  (show-recur x port mode))

(define (show-array x port mode)
  (case mode
    [(#t) (write x port)]
    [(#f) (display x port)]
    [else
     (if (array-representation-vector)
         (racket:for ([e x]
                      [i (racket:in-naturals)])
                     (fprintf port "~n    ~a:" i)
                     (show-bitvector e port mode))
         (begin
           (fprintf port " ")
           (print x port mode)))]))
