#lang racket/base

(require racket/function)

(provide (struct-out meta)
         dummy-metadata)

(struct meta (new-symbolic new-zeroed step input input* input-getters with-input output output* output-getters get-input get-output)
  #:transparent)

(define dummy-metadata
  (meta
   (thunk #t)
   (thunk #t)
   identity
   (thunk #t)
   (lambda args #t)
   '()
   (lambda (s i) s)
   (thunk #t)
   (lambda args #t)
   '()
   (lambda (s) #t)
   (lambda (s) #t)))
