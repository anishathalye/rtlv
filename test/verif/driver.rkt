#lang rosette/safe

(require
 (except-in "../yosys/verilog/counter.rkt" step)
 "driver/counter.rkt"
 (only-in verif/interpreter step run)
 rackunit)

(test-case "basic"
  (define c0 (new-zeroed-counter_s))
  (define s0 ((interpreter-factory metadata)
              '(let ()
                 (inc 10)
                 (get))
              c0))
  (check-equal? (run s0) 10))

(test-case "symbolic inputs and symbolic control flow"
  (define c0 (new-zeroed-counter_s))
  (define-symbolic* b boolean?)
  (define s0 ((interpreter-factory metadata)
              `(let ()
                 (out (input ,b #t))
                 (tick)
                 (if (not (bvzero? (output-count (in))))
                     (inc1)
                     (void))
                 (if (not ,b) (inc1) (void))
                 (get))
              c0))
  (define v (run s0))
  (define p (equal? v (if b 2 1)))
  (check-pred unsat? (verify (assert p))))

(test-case "concrete list input"
  (define c0 (new-zeroed-counter_s))
  (define s0 ((interpreter-factory metadata)
              '(let ()
                 (inc-all (list 1 2 3 4))
                 (get))
              c0))
  (check-equal? (run s0) 10))

(test-case "injected value, with a symbolic, causing symbolic control flow"
  (define c0 (new-zeroed-counter_s))
  (define-symbolic* b boolean?)
  (define x (if b 1 2))
  (define inp (list 1 x 1))
  (define s0 ((interpreter-factory metadata)
              `(let ()
                 (inc-all (value ,inp))
                 (get))
              c0))
  (define v (run s0))
  (define p (equal? v (if b 3 4)))
  (check-pred unsat? (verify (assert p))))

(test-case "simple verification, identify possible wraparound"
  (define c0 (new-symbolic-counter_s))
  (define s0 ((interpreter-factory metadata)
              ;; we write a program that returns true if it detects wraparound
              '(let ([initial (get)])
                 (inc1)
                 (cons ((get) . < . initial) initial))
              c0))
  (define v (run s0))
  (define m (solve (assert (car v))))
  (check-pred sat? m)
  (check-equal? (evaluate (cdr v) m) 255))
