#lang rosette/safe

(require (for-syntax syntax/parse
                     racket/syntax)
         syntax/parse/define)

; we need to be careful of what we import for runtime, because
; whatever we use needs to be compatible with Rosette
(require
 (prefix-in
  racket:
  (only-in racket/base
           in-range
           for/list
           struct-copy))
  memo)

; XXX this seems inefficient
(define (vector-update vec pos v)
  (define vec* (apply vector (vector->list vec)))
  (vector-set! vec* pos v)
  vec*)

(begin-for-syntax
  (define-splicing-syntax-class yosys-type
    #:attributes (ctor zero-ctor)
    (pattern (~datum Bool)
             #:attr ctor (lambda (name-stx)
                           #`(let ()
                               (define-symbolic* #,name-stx boolean?)
                               #,name-stx))
             #:attr zero-ctor (lambda (name-stx) #`#f))
    (pattern ((~datum _) (~datum BitVec) num:nat)
             #:attr ctor (lambda (name-stx)
                           #`(let ()
                               (define-symbolic* #,name-stx (bitvector num))
                               #,name-stx))
             #:attr zero-ctor (lambda (name-stx) #`(bv 0 num)))
    (pattern ((~datum Array) ((~datum _) (~datum BitVec) depth:nat) ((~datum _) (~datum BitVec) width:nat))
             #:attr ctor (lambda (name-stx)
                           #`(list->vector
                              (racket:for/list ([i (racket:in-range (expt 2 depth))])
                                (let () (define-symbolic* #,name-stx (bitvector width)) #,name-stx))))
             #:attr zero-ctor (lambda (name-stx)
                           #`(list->vector
                              (racket:for/list ([i (racket:in-range (expt 2 depth))])
                                (bv 0 width))))))

  (define-splicing-syntax-class yosys-member
    #:attributes (external-name name ctor zero-ctor)
    (pattern (~seq (name:id type:yosys-type) external-name:id)
             #:attr ctor ((attribute type.ctor) #'name)
             #:attr zero-ctor ((attribute type.zero-ctor) #'name)))

  (define-syntax-class yosys-initial-state
    #:attributes (name ctor zero-ctor)
    (pattern (name:id (~datum Bool))
             #:attr ctor #'(let () (define-symbolic* name boolean?) name)
             #:attr zero-ctor #'#f)))

(define-syntax (declare-datatype stx)
  (syntax-parse stx
    [(_ datatype-name:id ((_
                           init:yosys-initial-state
                           member:yosys-member ...)))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'datatype-name)
     #:with new-symbolic-name (format-id stx "new-symbolic-~a" #'datatype-name)
     #:with new-zeroed-name (format-id stx "new-zeroed-~a" #'datatype-name)
     #:with name-assoc (format-id stx "~a-name-assoc" #'datatype-name)
     #:with /... (quote-syntax ...)
     #`(begin
         (struct datatype-name (init.name member.external-name ...)
           #:transparent)
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
         #,@(for/list ([internal-name (syntax->list #'(init.name member.name ...))]
                       [external-name (syntax->list #'(init.name member.external-name ...))])
             (define getter (format-id stx "~a-~a" #'datatype-name external-name))
             #`(begin
                 (define (#,internal-name x) (#,getter x))
                 (provide #,getter)))
         (define (new-symbolic-name)
           (datatype-name init.ctor member.ctor ...))
         (provide new-symbolic-name)
         (define (new-zeroed-name)
           (datatype-name init.zero-ctor member.zero-ctor ...))
         (provide new-zeroed-name))]))
(provide declare-datatype)

(define-syntax (define-fun stx)
  (syntax-parse stx
    ; regular case
    [(_ name:id ((input:id input-type)) return-type body:expr)
     #'(begin
         (define/memoize (name input)
           body)
         (provide name))]
    ; transition function: treated specially
    ; case 1: when module is purely combinatorial
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        (~datum true))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'type)
     #'(begin
         (define (name state)
           (internal-copy-name type state))
         (provide name))]
    ; case 2: when state has a single field
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        ((~datum =) e (field:id (~datum next_state))))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'type)
     #`(begin
         (define (name state)
           (internal-copy-name type state [field e]))
         (provide name))]
    ; case 3: when state has multiple fields
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        ((~datum and) ((~datum =) e (field:id (~datum next_state))) ...))
     #:with internal-copy-name (format-id stx "internal-copy-~a" #'type)
     #`(begin
         (define (name state)
           (internal-copy-name type state [field e] ...))
         (provide name))]))
(provide define-fun)

(define (extractor i j)
  (lambda (x) (extract i j x)))

(define-simple-macro (_ (~datum extract) i:expr j:expr)
  (extractor i j))
(provide _)

(define (ite c t f)
  (if c t f))
(provide ite)

(define (distinct x y)
  (not (equal? x y)))
(provide distinct)

(define (select a i)
  (vector-ref a (bitvector->natural i)))
(provide select)

(define (store a i v)
  (vector-update a (bitvector->natural i) v))
(provide store)

(define (= x y)
  (equal? x y))
(provide =)

; to match SMTLIB's xor, which can take multiple arguments
(define (varargs-xor . args)
  (foldl xor #f args))
(provide (rename-out [varargs-xor xor]))

; no interpretation yet
(define-simple-macro (yosys-smt2-stdt)
  (void))
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

; no interpretation yet
(define-simple-macro (yosys-smt2-input name:id width:nat)
  (void))
(provide yosys-smt2-input)

; no interpretation yet
(define-simple-macro (yosys-smt2-output name:id width:nat)
  (void))
(provide yosys-smt2-output)

; no interpretation yet
(define-simple-macro (yosys-smt2-register name:id width:nat)
  (void))
(provide yosys-smt2-register)

; no interpretation yet
(define-simple-macro (yosys-smt2-memory name:id bits:nat width:nat read-ports:nat write-ports:nat 'sync:id)
  (void))
(provide yosys-smt2-memory)
