#lang rosette/safe

(require (for-syntax syntax/parse
                     racket/syntax)
         syntax/parse/define)

; we need to be careful of what we import for runtime, because
; whatever we use needs to be compatible with Rosette
(require
         (only-in racket/base
                  in-range
                  for/list
                  struct-copy)
         memo)

; XXX this seems inefficient
(define (vector-update vec pos v)
  (define vec* (apply vector (vector->list vec)))
  (vector-set! vec* pos v)
  vec*)

(begin-for-syntax
  (define-splicing-syntax-class yosys-type
    #:attributes (ctor)
    (pattern (~datum Bool)
             #:attr ctor (lambda (name-stx)
                           #`(let ()
                               (define-symbolic* #,name-stx boolean?)
                               #,name-stx)))
    (pattern ((~datum _) (~datum BitVec) num:nat)
             #:attr ctor (lambda (name-stx)
                           #`(let ()
                               (define-symbolic* #,name-stx (bitvector num))
                               #,name-stx)))
    (pattern ((~datum Array) ((~datum _) (~datum BitVec) depth:nat) ((~datum _) (~datum BitVec) width:nat))
             #:attr ctor (lambda (name-stx)
                           #`(list->vector
                              (for/list ([i (in-range (expt 2 depth))])
                                (let () (define-symbolic* #,name-stx (bitvector width)) #,name-stx))))))

  (define-splicing-syntax-class yosys-member
    #:attributes (external-name name ctor kw)
    (pattern (~seq (name:id type:yosys-type) external-name:id)
             #:attr kw (datum->syntax #'name (string->keyword (symbol->string (syntax-e #'name))))
             #:attr ctor ((attribute type.ctor) #'name)))

  (define-syntax-class yosys-initial-state
    #:attributes (name ctor kw)
    (pattern (name:id (~datum Bool))
             #:attr kw (datum->syntax #'name (string->keyword (symbol->string (syntax-e #'name))))
             #:attr ctor #'(let () (define-symbolic* name boolean?) name))))

(define-syntax (declare-datatype stx)
  (syntax-parse stx
    [(_ datatype-name:id ((_
                           init:yosys-initial-state
                           member:yosys-member ...)))
     #:with new-symbolic-name (format-id stx "new-symbolic-~a" #'datatype-name)
     #`(begin
         (struct datatype-name (init.name member.name ...)
           #:transparent)
         (provide datatype-name)
         #,@(for/list ([member-name (syntax->list #'(init.name member.name ...))])
             (define getter (format-id stx "~a-~a" #'datatype-name member-name))
             #`(begin
                 (define (#,member-name x) (#,getter x))
                 (provide #,getter)
                 (provide #,member-name)))
         (define (new-symbolic-name init.kw [init.name init.ctor]
                                    (~@ member.kw [member.name member.ctor]) ...)
           (datatype-name init.name member.name ...))
         (provide new-symbolic-name))]))
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
     #'(begin
         (define (name state)
           (struct-copy type state))
         (provide name))]
    ; case 2: when state has a single field
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        ((~datum =) e (field:id (~datum next_state))))
     #:with new-symbolic-name (format-id stx "new-symbolic-~a" #'type)
     #`(begin
         (define (name state)
           (new-symbolic-name #,(string->keyword (symbol->string (syntax-e #'field))) e))
         (provide name))]
    ; case 3: when state has multiple fields
    [(_ name:id ((state:id type:id) ((~datum next_state) next-type:id)) (~datum Bool)
        ((~datum and) ((~datum =) e (field:id (~datum next_state))) ...))
     #:with new-symbolic-name (format-id stx "new-symbolic-~a" #'type)
     #:with (kw ...) (datum->syntax
                      #'name
                      (for/list ([f (syntax->list #'(field ...))])
                        (string->keyword (symbol->string (syntax-e f)))))
     #`(begin
         (define (name state)
           (new-symbolic-name (~@ kw e) ...))
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
