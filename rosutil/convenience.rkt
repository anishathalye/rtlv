#lang racket/base

(require racket/match racket/contract syntax/parse/define
         (prefix-in @ (combine-in rosette/safe (only-in rosette/base/core/type typed? get-type type-deconstruct))))

(provide
 (rename-out [fresh-memory-like fresh-memory-like/unchecked])
 (contract-out
  [fresh-symbolic (-> (or/c symbol? string?)
                      @solvable?
                      @constant?)]
  [fresh-memory-like (-> (or/c symbol? string?)
                         vector?
                         (vectorof @constant?))]
  [concrete-head? (-> any/c
                      boolean?)]
  [concrete? (-> any/c boolean?)]))

(define name-count (make-hash))

; instantiate a fresh symbolic that prints with the given name
;
; Rosette compares constant? terms with equal? (when going into the term cache etc)
; so we create a fresh uninterned symbol with a given string representation to create
; a fresh constant
(define (fresh-symbolic name type)
  (define sym-base
    (if (symbol? name)
        (symbol->string name)
        name))
  (define counter (hash-ref name-count sym-base 0))
  (hash-set! name-count sym-base (add1 counter))
  ; make names that look like the ones that come out of define-symbolic*, but a little different
  ; so x%0 instead of x$0
  (@constant (string->uninterned-symbol (format "~a%~a" sym-base counter)) type))

; instantiate a vector of fresh symbolics, in the same form as a Yosys memory
(define (fresh-memory-like name mem)
  (let ([elem-type (@type-of (vector-ref mem 0))])
    (build-vector
     (vector-length mem)
     (lambda (i) (fresh-symbolic (format "~a[~a]" name i) elem-type)))))

(define (concrete-head? expr)
  (not (or (@term? expr) (@union? expr))))

;; TODO replace this with built-in concrete? in Rosette 4
;; see https://github.com/emina/rosette/pull/183
(define (concrete? val)
  (match val
    [(? @union?) #f]
    [(? @expression?) #f]
    [(? @constant?) #f]
    [(box v) (concrete? v)]
    [(? list?) (for/and ([v val]) (concrete? v))]
    [(cons x y) (and (concrete? x) (concrete? y))]
    [(vector vs ...) (for/and ([v vs]) (concrete? v))]
    [(and (? @typed?) (app @get-type t))
     (match (@type-deconstruct t val)
       [(list (== val)) #t]
       [components (for/and ([v components]) (concrete? v))])]
    [_ #t]))
