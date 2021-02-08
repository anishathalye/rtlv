#lang verif/driver

(define (store secret password)
  (out (input* 'op #t 'en #t 'secret secret 'password password))
  (tick)
  (out* 'en #f)
  #t)

(define (get guess)
  (out (input* 'op #f 'en #t 'secret (bv 0 128) 'password guess))
  (tick)
  (let ([secret (output-out (in))])
    (out* 'en #f)
    (tick)
    (yield get-fp) ; for demo purposes
    (hint merge)
    secret))

(define (recover)
  (tick))
