#lang typed/racket

(require "compiler.rkt")

(define args (vector->list (current-command-line-arguments)))
(display (visualize-CFG (first args) (second args)))