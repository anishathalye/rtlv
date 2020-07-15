#lang rosette/safe

(require
 (for-syntax syntax/parse racket/syntax)
 syntax/parse/define
 ; we need to be careful of what we import for runtime, because
 ; whatever we use needs to be compatible with Rosette
 "memoize.rkt" "parameters.rkt" "generic.rkt"
 rosutil
 (prefix-in ! racket/base))

(provide
 declare-datatype define-fun
 yosys-smt2-stdt yosys-smt2-module yosys-smt2-topmod
 yosys-smt2-clock yosys-smt2-input yosys-smt2-output
 yosys-smt2-register yosys-smt2-memory)

(begin-for-syntax
  (define-splicing-syntax-class yosys-type
    #:attributes (show ctor zero-ctor)
    (pattern (~datum Bool)
             #:with show #'show-boolean
             #:with ctor #'(lambda (name) (fresh-symbolic name boolean?))
             #:with zero-ctor #'#f)
    (pattern ((~datum _) (~datum BitVec) num:nat)
             #:with show #'show-bitvector
             #:with ctor #'(lambda (name) (fresh-symbolic name (bitvector num)))
             #:with zero-ctor #'(bv 0 num))
    (pattern ((~datum Array) ((~datum _) (~datum BitVec) depth:nat) ((~datum _) (~datum BitVec) width:nat))
             #:with show #'show-array
             #:with ctor #'(lambda (name)
                             (if (array-representation-vector)
                                 ; vector representation
                                 (!build-vector (expt 2 depth)
                                                (lambda (_)
                                                  (fresh-symbolic name (bitvector width))))
                                 ; UF representation
                                 (fresh-symbolic name (~> (bitvector depth) (bitvector width)))))
             #:with zero-ctor #'(if (array-representation-vector)
                                    ; vector representation
                                    (!build-vector (expt 2 depth) (lambda (_) (bv 0 width)))
                                    ; UF representation
                                    (lambda (addr) (bv 0 width)))))

  (define-splicing-syntax-class yosys-member
    #:attributes (external-name name show ctor zero-ctor)
    (pattern (~seq (name:id type:yosys-type) external-name:id)
             #:with show #'type.show
             #:with ctor #'(type.ctor 'external-name)
             #:with zero-ctor #'type.zero-ctor))

  (define-syntax-class yosys-initial-state
    #:attributes (name ctor zero-ctor)
    (pattern (name:id (~datum Bool))
             #:with ctor #'(fresh-symbolic 'name boolean?)
             #:with zero-ctor #'#f)))

(define-syntax (declare-datatype stx)
  (syntax-parse stx
    [(_ datatype-name:id ((_
                           init:yosys-initial-state
                           member:yosys-member ...)))
     #:with make-name (format-id stx "make-~a" #'datatype-name)
     #:with update-name (format-id stx "update-~a" #'datatype-name)
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'datatype-name)
     #:with new-symbolic-name (format-id stx "new-symbolic-~a" #'datatype-name)
     #:with new-zeroed-name (format-id stx "new-zeroed-~a" #'datatype-name)
     #:with init-getter (format-id stx "~a-~a" #'datatype-name #'init.name)
     #:with (getter ...) (for/list ([external-name (syntax->list #'(member.external-name ...))])
                           (format-id stx "~a-~a" #'datatype-name external-name))
     #:with /... (quote-syntax ...)
     #'(begin
         ; struct declaration
         (struct datatype-name (init.name member.external-name ...)
           #:methods gen:custom-write
           [(define (write-proc x port mode) (show x port mode))]
           #:methods gen:yosys-module
           [(define (fields _)
              (list 'init.name 'member.external-name ...))
            (define (get-field x field-name)
              (case field-name
                [(init.name) (init-getter x)]
                [(member.external-name) (getter x)] ...))
            (define (map-fields x f)
              (datatype-name
               (f 'init.name (init-getter x))
               (f 'member.external-name (getter x)) ...))]
           #:transparent)
         (define (show x port mode)
           (let ([include? (print-filter)])
             (cond
               [(boolean? mode)
                ; write or display
                (fprintf port "#(struct:~a" 'datatype-name)
                (when (include? 'init.name)
                  (fprintf port " ")
                  (show-boolean (init-getter x) port mode))
                (when (include? 'member.external-name)
                  (fprintf port " ")
                  (member.show (getter x) port mode)) ...
                (fprintf port ")")]
               [else
                ; print something more human-readable
                (fprintf port "~a {~n" 'datatype-name)
                (when (include? 'member.external-name)
                  (fprintf port "  ~a:" 'member.external-name)
                  (member.show (getter x) port mode)
                  (fprintf port "\n")) ...
                (fprintf port "}")])))
         (provide datatype-name)

         ; like struct-copy, but uses the internal names instead of external names
         ; for transition function
         (begin-for-syntax
           (define name-assoc
             (make-hash '((member.name . member.external-name) ...))))
         (define-syntax (internal-copy-name stx)
           (syntax-parse stx
             [(_ state [internal-name value] /...)
              #`(!struct-copy
                 datatype-name
                 state
                 #,@(for/list ([i (syntax->list #'(internal-name /...))]
                               [v (syntax->list #'(value /...))])
                      #`(#,(datum->syntax #'datatype-name (hash-ref name-assoc (syntax-e i))) #,v)))
              ]))

         ; getters
         (define init.name init-getter)
         (define member.name getter) ... ; for internal use only, not exported
         (define member.external-name getter) ...
         (provide getter) ...

         ; constructors
         (define (new-symbolic-name)
           (datatype-name init.ctor member.ctor ...))
         (provide new-symbolic-name)
         (define (new-zeroed-name)
           (datatype-name init.zero-ctor member.zero-ctor ...))
         (provide new-zeroed-name)
         (define-syntax update-name
           (syntax-parser
             [(_ struct-expr:expr [field:id value:expr] /...)
              #'(update-fields struct-expr (list (cons 'field value) /...))]))
         (provide update-name)
         (define-syntax make-name
           (syntax-parser
             [(_ [field:id value:expr] /...)
              #'(update-name (new-zeroed-name) [field value] /...)]))
         (provide make-name))]))

(define define-fun-hooks '())

(define (add-define-fun-hook hook)
  (set! define-fun-hooks (cons hook define-fun-hooks)))

(define (trigger-hooks name fn)
  (!for ([hook define-fun-hooks])
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
            (internal-copy-name state)))
         (provide name)
         (trigger-hooks 'name name))]
    ; case 2: when state has a single field
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        ((~datum =) e (field:id (~datum next_state))))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'type)
     #'(begin
         (define (name state)
           (new-memoization-context
            (internal-copy-name state [field e])))
         (provide name)
         (trigger-hooks 'name name))]
    ; case 3: when state has multiple fields
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        ((~datum and) ((~datum =) e (field:id (~datum next_state))) ...))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'type)
     #'(begin
         (define (name state)
           (new-memoization-context
            (internal-copy-name state [field e] ...)))
         (provide name)
         (trigger-hooks 'name name))]))

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

; no interpretation yet
(define-simple-macro (yosys-smt2-module name:id)
  (void))

; no interpretation yet
(define-simple-macro (yosys-smt2-topmod name:id)
  (void))

; no interpretation yet
(define-simple-macro (yosys-smt2-clock name:id 'edge:id)
  (void))

(define-simple-macro (make-appender lst)
  (lambda (name fn) (set! lst (cons (cons name fn) lst))))

(define-syntax (yosys-smt2-input stx)
  (syntax-parse stx
    [(_ name:id width:nat)
     #:with inputs (format-id stx "inputs")
     #'(add-define-fun-hook (make-appender inputs))]))

(define-syntax (yosys-smt2-output stx)
  (syntax-parse stx
    [(_ name:id width:nat)
     #:with outputs (format-id stx "outputs")
     #'(add-define-fun-hook (make-appender outputs))]))

(define-syntax (yosys-smt2-register stx)
  (syntax-parse stx
    [(_ name:id width:nat)
     #:with registers (format-id stx "registers")
     #'(add-define-fun-hook (make-appender registers))]))

(define-syntax (yosys-smt2-memory stx)
  (syntax-parse stx
    [(_ name:id bits:nat width:nat read-ports:nat write-ports:nat 'sync:id)
     #:with memories (format-id stx "memories")
     #'(add-define-fun-hook (make-appender memories))]))

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
         (!for ([e x]
                [i (!in-naturals)])
               (fprintf port "~n    ~a:" i)
               (show-bitvector e port mode))
         (begin
           (fprintf port " ")
           (print x port mode)))]))
