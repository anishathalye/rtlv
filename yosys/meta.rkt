#lang racket/base

(provide (struct-out meta))

(struct meta (step input input* input-getters with-input output output-getters get-output)
  #:transparent)
