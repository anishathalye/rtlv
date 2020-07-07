#lang racket/base

(require "../verilog.rkt"
         racket/pretty
         (for-syntax racket/base syntax/parse))

(provide (except-out (all-from-out "../verilog.rkt") #%module-begin)
         (rename-out [yosys-debug-expand-module-begin #%module-begin]))

(define-syntax (yosys-debug-expand-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #'(#%module-begin
        (let ([expanded (syntax->datum (expand-syntax-once #'form))])
          (unless (equal? expanded '(void))
            (pretty-print expanded (current-output-port) 1))) ...)]))
