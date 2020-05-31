#lang racket

(require (prefix-in r: rosette))

(define (concretize term [error-on-failure #f])
  (let/ec return
    (define vars (r:symbolics term))
    (when (empty? vars) (return term))
    (define model (r:solve #t))
    (when (not (r:sat? model))
      (error "can't find single concrete state"))
    (define term-concrete (r:evaluate term (r:complete-solution model (r:symbolics term))))
    (define must-be-same
      (r:unsat?
       (r:verify
        (r:assert
         (r:equal? term term-concrete)))))
    (cond
      [must-be-same term-concrete]
      [(not error-on-failure) term]
      [else (error 'concretize "failed to concretize term")])))
(provide concretize)
