#lang racket/base

(require (prefix-in @ rosette/safe)
         syntax/parse/define)

(provide (rename-out
          [$#%module-begin #%module-begin])
         ; from Rosette
         (rename-out
          [@#%top-interaction #%top-interaction]
          [@#%app #%app]
          [@#%datum #%datum]
          [@#%top #%top]))

(define-simple-macro ($#%module-begin form ...)
  (@#%module-begin
   (module configure-runtime racket/base
     (require yosys/lang/configure-runtime)
     (configure-runtime!))
   form ...))
