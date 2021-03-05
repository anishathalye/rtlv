#lang racket/base

(require
 "parameters.rkt"
 racket/generic racket/contract racket/function racket/match racket/format
 (for-syntax racket/base syntax/parse racket/syntax)
 (prefix-in @ rosette/safe))

(provide
 gen:yosys-module yosys-module? yosys-module/c
 gen:dynamically-addressable dynamically-addressable? dynamically-addressable/c
 (contract-out
  [fields (-> dynamically-addressable? (listof symbol?))]
  [field-type (-> dynamically-addressable? symbol? any)]
  [get-field (-> dynamically-addressable? symbol? any)]
  [map-fields (-> dynamically-addressable? (-> symbol? any/c any) dynamically-addressable?)]
  [update-fields (-> dynamically-addressable? (listof (cons/c symbol? any/c)) dynamically-addressable?)]
  [update-field (-> dynamically-addressable? symbol? any/c dynamically-addressable?)]
  [show-diff (-> dynamically-addressable? dynamically-addressable? string?)])
 for/struct
 dynamically-addressable-struct)

;; just a tag
(define-generics yosys-module)

(define-generics dynamically-addressable
  [fields dynamically-addressable]
  [field-type dynamically-addressable field]
  [get-field dynamically-addressable field]
  [map-fields dynamically-addressable fn]
  [update-fields dynamically-addressable assoc]
  #:fallbacks
  [(define/generic gen-map-fields map-fields)
   (define (update-fields x assoc)
     (define h (make-immutable-hasheq assoc))
     (gen-map-fields x (lambda (name old-value) (hash-ref h name (lambda () old-value)))))])

(define-syntax for/struct
  (syntax-parser
    [(_ [v:id s] body ...)
     #'(for/struct [(_ v) s] body ...)]
    [(_ [(k:id v:id) s] body ...)
     #'(map-fields s (lambda (k v) body ...))]))

(define (update-field struct-value field-name new-value)
  (update-fields struct-value (list (cons field-name new-value))))

(define (show-diff self other)
  (define out-string (open-output-string))
  (parameterize ([current-output-port out-string])
    (define include? (field-filter))
    (printf "{\n")
    (for ([f (fields self)])
      (when (and (include? f)
                 (not (equal? (get-field self f) (get-field other f))))
        (define field-name (symbol->string f))
        (match (field-type self f)
          [(list 'array depth width)
           (printf "  ~a:\n" field-name)
           (for ([i (in-range (expt 2 depth))])
             (define s-i (vector-ref (get-field self f) i))
             (define o-i (vector-ref (get-field other f) i))
             (when (not (equal? s-i o-i))
               (printf "    ~a: - ~v\n" i s-i)
               (printf "    ~a  + ~v\n" (~a "" #:width (string-length (~a i))) o-i)))]
          [else
           ;; diff boolean or bit vector
           (printf "  ~a: - ~v\n" field-name (get-field self f))
           (printf "  ~a  + ~v\n" (~a "" #:width (string-length field-name)) (get-field other f))])))
    (printf "}"))
  (get-output-string out-string))

(define-syntax (dynamically-addressable-struct stx)
  (syntax-parse stx
    [(_ name:id ([field-name:id type-descriptor:expr] ...) struct-option ...)
     #:with (getter ...) (for/list ([f (syntax->list #'(field-name ...))])
                           (format-id #'name "~a-~a" #'name f))
     #'(@struct name (field-name ...) struct-option ...
                #:methods gen:dynamically-addressable
                [(define (fields _)
                   (list 'field-name ...))
                 (define (get-field x f)
                   (define v (@case f [(field-name) (getter x)] ...))
                   (@if (@void? v)
                        (error 'get-field "no such field: ~a" f)
                        v))
                 (define (field-type x f)
                   (define v (@case f [(field-name) type-descriptor] ...))
                   (@if (@void? v)
                        (error 'field-type "no such field: ~a" f)
                        v))
                 (define (map-fields x f)
                   (name (f 'field-name (getter x)) ...))])]))
