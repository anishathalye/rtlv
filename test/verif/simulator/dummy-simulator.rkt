#lang verif/simulator

(struct state (dummy circuit))

(define (init)
  (set! (state 1234 (circuit-new))))

(define (inc-and-get!)
  (let ([c0 (state-circuit (get))])
    (let ([c1 (circuit-with-input c0 (input* 'nrst #t 'en #t))])
      (let ([c2 (circuit-step c1)])
        (set! (state (state-dummy (get)) c2))
        (output-count (circuit-get-output c2))))))
