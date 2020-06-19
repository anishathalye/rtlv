#lang racket/base

(require syntax/parse/define)

(define-simple-macro (require/provide module-path:str ...)
  (begin
    (require module-path ...)
    (provide (all-from-out module-path) ...)))

(require/provide
 "racket-api.rkt"
 "concretize.rkt"
 "convenience.rkt"
 "debug.rkt")
