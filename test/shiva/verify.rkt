#lang racket/base

(require "verilog/soc.rkt"
         racket/list racket/port racket/string racket/function racket/match
         (prefix-in @ rosette/safe)
         yosys shiva
         rackunit)

(overapproximate-symbolic-load-threshold 64)
(overapproximate-symbolic-store-threshold 64)

(define inputs
  `((gpio_pin_in . ,(@bv 0 8))
    (uart_rx . ,(@bv #b1111 4))))

(define statics
  ; for some reason, the picorv32 has a physical register for x0/zero,
  ; cpuregs[0], whose value can never change in practice
  '((cpu.cpuregs 0)))

(define (hints q . args)
  (match q
    ['statics statics]
    [_ #f]))

(test-case "verify-deterministic-start: limit"
  (define ret #f)
  (define output
    (with-output-to-string
      (thunk
       (set!
        ret
        (verify-deterministic-start
         new-symbolic-soc_s
         #:invariant soc_i
         #:step soc_t
         #:reset 'resetn
         #:reset-active 'low
         #:inputs inputs
         #:state-getters (append registers memories)
         #:hints hints
         #:print-style 'names
         #:try-verify-after 450
         #:limit 450)))))
  (check-false ret))

(test-case "verify-deterministic-start: soc"
  (define ncycles #f)
  (define output
    (with-output-to-string
      (thunk
       (set!
        ncycles
        (verify-deterministic-start
         new-symbolic-soc_s
         #:invariant soc_i
         #:step soc_t
         #:reset 'resetn
         #:reset-active 'low
         #:inputs inputs
         #:state-getters (append registers memories)
         #:hints hints
         #:print-style 'names
         #:try-verify-after 450)))))
  (check-true (@vc-assumes (@vc)))
  (check-true (@vc-asserts (@vc)))
  (check-equal? ncycles 472)
  (check-true (string-contains? output "-> sat!"))
  (check-true (string-contains? output "cycle 472"))
  (check-true (string-contains? output "-> unsat!"))
  (check-false (string-contains? output "cycle 473")))

(test-case "verify-deterministic-start: soc input/output"
  (define inputs
    '(gpio_pin_in uart_rx))
  (define ncycles #f)
  (define output
    (with-output-to-string
      (thunk
       (set!
        ncycles
        (verify-deterministic-start
         new-symbolic-soc_s
         #:invariant soc_i
         #:step soc_t
         #:reset 'resetn
         #:reset-active 'low
         #:inputs inputs
         #:state-getters (append registers memories)
         #:output-getters (list (cons '|soc_n uart_tx| |soc_n uart_tx|))
         #:hints hints
         #:print-style 'names
         #:try-verify-after 450)))))
  (check-true (@vc-assumes (@vc)))
  (check-equal? ncycles 472)
  (check-true (string-contains? output "-> sat!"))
  (check-true (string-contains? output "cycle 472"))
  (check-true (string-contains? output "-> unsat!"))
  (check-false (string-contains? output "cycle 473")))
