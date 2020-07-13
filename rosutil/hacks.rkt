#lang racket/base

(require racket/contract
         (prefix-in @ rosette/safe))

(provide
 (contract-out
  [term-cache-ephemeral! (-> void?)]
  [make-ephemeral-term-cache (->* ()
                                  ((or/c #f hash?))
                                  hash?)]))

(define (term-cache-ephemeral!)
  (@term-cache (make-ephemeral-term-cache (@term-cache))))

(define (make-ephemeral-term-cache [base-cache #f])
  (define new-cache (make-ephemeron-hash))
  (when base-cache
    (for ([(k v) base-cache])
      (hash-set! new-cache k v)))
  new-cache)

(define (ephemeron-hash-ref-proc-replace-value hash key value)
  (ephemeron-value value))

(define (ephemeron-hash-ref-proc hash key)
  (values key ephemeron-hash-ref-proc-replace-value))

(define (ephemeron-hash-set-proc hash key value)
  (values key (make-ephemeron key value)))

(define (ephemeron-hash-remove-proc hash key)
  key)

(define (ephemeron-hash-key-proc hash key)
  key)

(define (ephemeron-hash-clear-proc hash)
  (hash-clear! hash))

(define (make-ephemeron-hash)
  (impersonate-hash
   (make-weak-hash)
   ephemeron-hash-ref-proc
   ephemeron-hash-set-proc
   ephemeron-hash-remove-proc
   ephemeron-hash-key-proc
   ephemeron-hash-clear-proc))

(module+ test
  (require rackunit)

  (struct x (a) #:transparent)
  (struct y (b c) #:transparent)

  (test-case "make-hash: no gc"
    (define h (make-hash))
    (let ()
      (define key1 (x 1))
      (define value1 (y 2 3))
      (define key2 (x 4))
      (define value2 (y 5 key2)) ; refers back to key2 (self key)
      (define key3 (x 6))
      (define value3 (y 7 key1)) ; refers back to key1 (other key)

      (hash-set! h key1 value1)
      (hash-set! h key2 value2)
      (hash-set! h key3 value3)

      (collect-garbage)

      (check-equal? (hash-count h) 3)

      ; prevent collection before here
      (hash-ref h key1)
      (hash-ref h key2)
      (hash-ref h key3)
      (void))

    (check-equal? (hash-count h) 3))

  (test-case "make-weak-hash: partial gc"
    (define h (make-weak-hash))
    (let ()
      (define key1 (x 1))
      (define value1 (y 2 3))
      (define key2 (x 4))
      (define value2 (y 5 key2)) ; refers back to key2 (self key)
      (define key3 (x 6))
      (define value3 (y 7 key1)) ; refers back to key1 (other key)

      (hash-set! h key1 value1)
      (hash-set! h key2 value2)
      (hash-set! h key3 value3)

      (collect-garbage)

      (check-equal? (hash-count h) 3)

      ; prevent collection before here
      (hash-ref h key1)
      (hash-ref h key2)
      (hash-ref h key3)
      (void))

    (collect-garbage)

    ; technically, I don't think (collect-garbage)'s spec guarantees that
    ; it always collects everything, but this test seems to work in practice
    (check-equal? (hash-count h) 2))

  (test-case "make-ephemeron-hash: full gc"
    (define h (make-ephemeron-hash))
    (let ()
      (define key1 (x 1))
      (define value1 (y 2 3))
      (define key2 (x 4))
      (define value2 (y 5 key2)) ; refers back to key2 (self key)
      (define key3 (x 6))
      (define value3 (y 7 key1)) ; refers back to key1 (other key)

      (hash-set! h key1 value1)
      (hash-set! h key2 value2)
      (hash-set! h key3 value3)

      (collect-garbage)

      (check-equal? (hash-count h) 3)

      ; prevent collection before here
      (hash-ref h key1)
      (hash-ref h key2)
      (hash-ref h key3)
      (void))

    (collect-garbage)

    ; technically, I don't think (collect-garbage)'s spec guarantees that
    ; it always collects everything, but this test seems to work in practice
    (check-equal? (hash-count h) 0)))
