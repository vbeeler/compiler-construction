#lang typed/racket

(require "compiler.rkt")

;; get the command line arguments
(define args (vector->list (current-command-line-arguments)))
;; compile the mini program using the given parsed json
(display (compile-json (first args) (second args) (third args) (fourth args) (fifth args) (sixth args)))