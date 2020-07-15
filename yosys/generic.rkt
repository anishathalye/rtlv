#lang racket/base

(require
 racket/generic racket/contract
 (for-syntax racket/base syntax/parse))

(provide
 gen:yosys-module yosys-module? yosys-module/c
 (contract-out
  [fields (-> yosys-module? (listof symbol?))]
  [get-field (-> yosys-module? symbol? any)]
  [map-fields (-> yosys-module? (-> symbol? any/c any) yosys-module?)]
  [update-fields (-> yosys-module? (listof (cons/c symbol? any/c)) yosys-module?)]
  [update-field (-> yosys-module? symbol? any/c yosys-module?)])
 for/struct)

(define-generics yosys-module
  [fields yosys-module]
  [get-field yosys-module field]
  [map-fields yosys-module fn]
  [update-fields yosys-module assoc]
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
     #:declare s (expr/c #'yosys-module?)
     #'(map-fields s.c (lambda (k v) body ...))]))

(define (update-field struct-value field-name new-value)
  (update-fields struct-value (list (cons field-name new-value))))
