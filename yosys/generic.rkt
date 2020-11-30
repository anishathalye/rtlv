#lang racket/base

(require
 "parameters.rkt"
 racket/generic racket/contract racket/function racket/match racket/format
 (for-syntax racket/base syntax/parse))

(provide
 gen:yosys-module yosys-module? yosys-module/c
 (contract-out
  [fields (-> yosys-module? (listof symbol?))]
  [field-type (-> yosys-module? symbol? any)]
  [get-field (-> yosys-module? symbol? any)]
  [map-fields (-> yosys-module? (-> symbol? any/c any) yosys-module?)]
  [update-fields (-> yosys-module? (listof (cons/c symbol? any/c)) yosys-module?)]
  [update-field (-> yosys-module? symbol? any/c yosys-module?)]
  [show-diff (-> yosys-module? yosys-module? any)])
 for/struct)

(define-generics yosys-module
  [fields yosys-module]
  [field-type yosys-module field]
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

(define (show-diff self other)
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
