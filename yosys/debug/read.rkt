#lang racket

(require racket/pretty
         (for-syntax syntax/parse))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [yosys-debug-read-module-begin #%module-begin]))

(define-syntax (yosys-debug-read-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #`(#%module-begin
        (pretty-print 'form (current-output-port) 1) ...)]))
