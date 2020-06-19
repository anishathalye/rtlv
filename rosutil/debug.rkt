#lang racket/base

(require racket/list racket/match
         (only-in rosette bv? expression constant symbolics union)
         (only-in rosette/base/core/term type-deconstruct get-type typed?))

(provide value-size value-depth find-large-terms)

; calculate a Rosette value's size
;
; the value returned by this function should match what you would
; get from calling render-value/snip, fully expand the value,
; and then counting the number of boxes. another way to think about
; this is that it returns the number of elements in the flattened
; version of the "list representation" of the value.
(define (value-size val)
  (value-fold val
              (lambda (v) 1)
              (lambda (v xs) (+ 1 (apply + xs)))))

; calculate a Rosette value's nesting depth
;
; the value returned by this function should match what you would
; get from calling render-value/snip, fully expand the value,
; and then counting depth of the deepest tree. another way to think about
; this is that it returns the maximum nesting depth in the "list representation"
; of the value.
(define (value-depth val)
  (value-fold val
              (lambda (v) 0)
              (lambda (v xs) (+ 1 (apply max xs)))))

(define (value-fold val leaf node)
  (define (rec children)
    (for/list ([child (in-list children)])
      (value-fold child leaf node)))
  ; mirrors the structure used in render-value/snip
  ; https://github.com/emina/rosette/blob/master/rosette/lib/value-browser.rkt
  (match val
    [(constant id type) (leaf val)]
    [(union gvs)
     (node val
           (for/fold ([acc '()]
                      #:result (reverse acc))
                     ([child (in-list gvs)])
             (define condition (car child))
             (define value (cdr child))
             (cons (value-fold value leaf node) (cons (value-fold condition leaf node) acc))))]
    [(expression op child ...)
     (node val (rec child))]
    [(list child ...)
     (node val (rec child))]
    [(vector child ...)
     (node val (rec child))]
    [(? box?)
     (node val (list (value-fold (unbox val) leaf node)))]
    [(cons a b)
     (node val (list (value-fold a leaf node)
                     (value-fold b leaf node)))]
    [(? integer?) (leaf val)]
    [(? real?) (leaf val)]
    [(? boolean?) (leaf val)]
    [(? typed?)
     (define t (get-type val))
     (match (type-deconstruct t val)
       [(list (== val))
        ;; typed value
        (match val
          [(? bv?) (leaf val)]
          [(? procedure?) (leaf val)]
          [_
           ;; this should be a dead code in principle
           (leaf val)]
          [vs
           (node val (rec vs))])])]
    ;; a struct could have prop:procedure, so this test should
    ;; follow the struct test
    [(? procedure?) (leaf val)]
    [_
     ; other
     (leaf val)]))

; finds suspicously large terms among the elements of a module's state
;
; state-getters has the same construction as the argument of the same name to
; verify-deterministic-start in rtl/shiva
;
; returns number of terms with value-depth >= threshold
; additionally outputs information about these terms if output? is #t
(define (find-large-terms state state-getters
                          #:threshold [threshold 3]
                          #:output? [output? #f])
  (define term-depths
    (for/list ([state-getter state-getters])
      (let* ([name (first state-getter)]
             [getter (second state-getter)]
             [term (getter state)]
             [depth (value-depth term)]
             [symvars (symbolics term)])
        (when (and output? (>= depth threshold))
          (printf "~a: depth: ~a symbolics: ~a ~n" name depth symvars))
        depth)))
  (define large-term-count
    (length (filter (lambda (n) (>= n threshold)) term-depths)))
  large-term-count)
