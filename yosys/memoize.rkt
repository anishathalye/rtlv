#lang racket

(require (for-syntax syntax/parse))

(provide new-memoization-context define/memoize1)

(define context (make-parameter #f))

(define (with-new-memoization-context proc)
  (parameterize ([context (make-hasheq)])
    (proc)))

(define-syntax (new-memoization-context stx)
  (syntax-parse stx
    [(_ body ...)
     #'(with-new-memoization-context (thunk body ...))]))

(define (memoize1 proc)
  (define (memoized arg)
    (let ([current-context (context)])
      (if current-context
          (let ([arg-value (hash-ref current-context memoized #f)])
            (if (and arg-value (eq? arg (car arg-value)))
                (cdr arg-value)
                (let ([value (proc arg)])
                  (hash-set! current-context memoized (cons arg value))
                  value)))
          (proc arg))))
  memoized)

(define-syntax (define/memoize1 stx)
  (syntax-parse stx
    [(_ (name:id arg:id) body ...)
     #'(define name (memoize1 (lambda (arg) body ...)))]))

(module+ test
  (require rackunit)

  (test-case "no context"
    (define run-count 0)
    (define/memoize1 (f x)
      (set! run-count (+ run-count 1))
      (* x 5))
    (check-equal? (f 3) 15)
    (check-equal? run-count 1)
    (check-equal? (f 3) 15)
    (check-equal? run-count 2))

  (test-case "basic context"
    (define run-count 0)
    (define/memoize1 (f x)
      (set! run-count (+ run-count 1))
      (* x 5))
    (new-memoization-context
     (check-equal? (f 3) 15)
     (check-equal? run-count 1)
     (check-equal? (f 3) 15)
     (check-equal? run-count 1)))

  (test-case "multiple values"
    (define run-count 0)
    (define/memoize1 (f x)
      (set! run-count (+ run-count 1))
      (* x 5))
    (new-memoization-context
     (check-equal? (f 3) 15)
     (check-equal? run-count 1)
     (check-equal? (f 4) 20)
     (check-equal? run-count 2)
     (check-equal? (f 4) 20)
     (check-equal? run-count 2)
     (check-equal? (f 3) 15)
     (check-equal? run-count 3))))
