#lang racket

(require "../main.rkt"
         (for-syntax syntax/parse))

(provide (except-out (all-from-out "../main.rkt") #%module-begin)
         (rename-out [yosys-debug-expand-module-begin #%module-begin]))

(define-syntax (yosys-debug-expand-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #`(#%module-begin
        (syntax->datum (expand-syntax-once #'form)) ...)]))
