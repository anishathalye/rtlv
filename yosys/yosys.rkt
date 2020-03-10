#lang rosette/safe

(require (for-syntax syntax/parse
                     racket/syntax)
         syntax/parse/define
         (only-in racket/base
                  in-range
                  for/list))

; XXX this seems inefficient
(define (vector-update vec pos v)
  (define vec* (apply vector (vector->list vec)))
  (vector-set! vec* pos v)
  vec*)

(begin-for-syntax
  (define-syntax-class yosys-member
    #:attributes (name ctor kw)
    (pattern (name:id (~datum Bool))
             #:attr kw (datum->syntax #'name (string->keyword (symbol->string (syntax-e #'name))))
             #:attr ctor #'(let () (define-symbolic* name boolean?) name))
    (pattern (name:id ((~datum _) (~datum BitVec) num:expr))
             #:attr kw (datum->syntax #'name (string->keyword (symbol->string (syntax-e #'name))))
             #:attr ctor #'(let () (define-symbolic* name (bitvector num)) name))
    (pattern (name:id ((~datum Array) ((~datum _) (~datum BitVec) depth:expr)
                               ((~datum _) (~datum BitVec) width:expr)))
             #:attr kw (datum->syntax #'name (string->keyword (symbol->string (syntax-e #'name))))
             #:attr ctor #'(list->vector (for/list ([i (in-range (expt 2 depth))])
                                           (let () (define-symbolic* name (bitvector width)) name))))))

(define-syntax (declare-datatype stx)
  (syntax-parse stx
    [(_ datatype-name:id ((_
                  member:yosys-member ...)))
     #:with new-symbolic-name (format-id stx "new-symbolic-~a" #'datatype-name)
     #`(begin
         (struct datatype-name (member.name ...)
           #:transparent)
         (provide datatype-name)
         #,@(for/list ([member-name (syntax->list #'(member.name ...))])
             (define getter (format-id stx "~a-~a" #'datatype-name member-name))
             #`(begin
                 (define (#,member-name x) (#,getter x))
                 (provide #,getter)
                 (provide #,member-name)))
         (define (new-symbolic-name (~@ member.kw [member.name member.ctor]) ...)
           (datatype-name member.name ...))
         (provide new-symbolic-name))]))
(provide declare-datatype)

(define-syntax (define-fun stx)
  (syntax-parse stx
    ; regular case
    [(_ name:id ((input:id input-type)) return-type body:expr)
     #'(begin
         (define (name input)
           body)
         (provide name))]
    ; transition function: treated specially
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
