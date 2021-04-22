#lang rosette/safe

(module ideal rosette/safe
  (require rosutil
           (only-in racket values))

  (provide (all-defined-out))

  (struct state (secret password) #:transparent)
  (define (new-symbolic-state)
    (state
     (fresh-symbolic "secret" (bitvector 128))
     (fresh-symbolic "password" (bitvector 128))))

  (define s0 (state (bv 0 128) (bv 0 128)))

  (define ((get guess) s)
    (define ret (if (equal? guess (state-password s)) (state-secret s) (bv 0 128)))
    (values ret s0))

  (define ((store secret password) s)
    (values #t (state secret password))))

(require (prefix-in ideal: 'ideal)
         "driver/lockbox.rkt"
         (except-in "../yosys/verilog/lockbox.rkt" step)
         (except-in verif/driver run*)
         yosys
         (only-in shiva with-invariants)
         (only-in racket/base for)
         (only-in racket/class new send)
         racket/match
         rackunit)

(define i0 (input #f #f (bv 0 128) (bv 0 128)))
(define c0 (with-input (with-invariants (new-symbolic-lockbox_s) lockbox_i) i0))

(define hint-db
  (make-hintdb
   [get-fp (fixpoint 0 (filter/or "count_cycle") 3 '())]
   [merge (merge (lambda (_) #f))]))

(define (run prog c0)
  (define state ((interpreter-factory metadata) prog c0))
  (define exc (new executor% [initial-state state] [hint-db hint-db]))
  (send exc run!))

(test-case "merging"
  (define c0 (with-invariants (new-symbolic-lockbox_s) lockbox_i))
  (define-symbolic* secret password (bitvector 128))
  (define prog
    `(begin
       (store ',secret ',password)
       (get ',password)))
  (define final (run prog c0))
  (check-equal? (length final) 2) ; make sure merge worked
  (for ([f final])
    (match-define (finished v c1) f)
    (check-pred unsat? (verify (assert (equal? v secret))))))

(define (R f ci)
  (and
   (equal? (ideal:state-secret f) (|lockbox_n stored_secret| ci))
   (equal? (ideal:state-password f) (|lockbox_n stored_password| ci))
   (equal? (|lockbox_n returned_secret| ci) (bv 0 128))
   (equal? (|lockbox_n en| ci) #f)
   (not (equal? (|lockbox_n x1| ci) (|lockbox_n x2| ci)))))

(define (test-step op prog)
  (define f1 (ideal:new-symbolic-state))
  (define c1 (new-symbolic-lockbox_s))
  (define-values (out-f f2) (op f1))
  (define final (run prog c1))
  (for ([f final])
    (match-define (finished v c2) f)
    (check-pred
     unsat?
     (verify
      (begin (assume (R f1 c1))
             (assert (and (equal? v out-f) (R f2 c2))))))))

(test-case "R initial"
  (check-pred unsat? (verify (assert (R ideal:s0 c0)))))

(test-case "R step: store"
  (define-symbolic secret password (bitvector 128))
  (test-step
   (ideal:store secret password)
   `(store ',secret ',password)))

(test-case "R step: get"
  (define-symbolic guess (bitvector 128))
  (test-step
   (ideal:get guess)
   `(get ',guess)))

(test-case "recovery not harmful"
  (define f1 (ideal:new-symbolic-state))
  (define c1 (new-symbolic-lockbox_s))
  (define final (run '(recover) c1))
  (for ([f final])
    (match-define (finished v c2) f)
    (check-pred unsat? (verify
                        (begin (assume (R f1 c1))
                               (assert (R f1 c2)))))))
