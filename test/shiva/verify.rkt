#lang rosette/safe

(require "verilog/picosoc.rkt"
         (only-in racket struct-copy for/list string-contains?)
         (only-in racket/port with-output-to-string)
         shiva
         rosutil
         syntax/parse/define
         rackunit)

(define (input-setter s)
  (struct-copy picosoc_s s
               [resetn #t]
               [button (bv 0 5)]
               [uart_rx (bv #b1111 4)]))

(define (init-input-setter s)
  (struct-copy picosoc_s s
               [resetn #f]
               [button (bv 0 5)]
               [uart_rx (bv #b1111 4)]))

(define (statics s)
  ; for some reason, the picosoc has a physical register for x0/zero,
  ; cpuregs[0], whose value can never change in practice
  (vector-ref (|picosoc_m cpu.cpuregs| s) 0))

(define-simple-macro (fresh-memory-like name mem)
  (list->vector
   (for/list
       ([i mem])
     (fresh-symbolic name (bitvector (bitvector-width i))))))

(define (overapproximate s cycle)
  (if (equal? cycle 4)
      ; overapproximate RAM/cpuregs behavior to avoid a big ite that doesn't matter
      (struct-copy picosoc_s s
                   [ram.ram (fresh-memory-like ram (|picosoc_m ram.ram| s))]
                   [cpu.cpuregs (fresh-memory-like cpuregs (|picosoc_m cpu.cpuregs| s))])
      #f))

(test-case "verify-deterministic-start: picosoc"
  (define output
    (with-output-to-string
      (lambda ()
        (verify-deterministic-start
         picosoc_s
         new-symbolic-picosoc_s
         #:invariant picosoc_i
         #:step picosoc_t
         #:init-input-setter init-input-setter
         #:input-setter input-setter
         #:state-getters (append registers memories)
         #:statics statics
         #:overapproximate overapproximate
         #:print-style 'names
         #:try-verify-after 450))))
  (check-true (string-contains? output "-> sat!"))
  (check-true (string-contains? output "cycle 473"))
  (check-true (string-contains? output "-> unsat!"))
  (check-false (string-contains? output "cycle 474")))
