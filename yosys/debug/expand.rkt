#lang racket

(require "../main.rkt"
         racket/pretty
         (for-syntax syntax/parse))

(provide (except-out (all-from-out "../main.rkt") #%module-begin)
         (rename-out [yosys-debug-expand-module-begin #%module-begin]))

(define-syntax (yosys-debug-expand-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #`(#%module-begin
        (let ([expanded (syntax->datum (expand-syntax-once #'form))])
          (when (not (equal? expanded '(void)))
            (pretty-print expanded (current-output-port) 1))) ...)]))
