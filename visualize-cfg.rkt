#lang typed/racket

(require "compiler.rkt")

;; get the command line arguments
(define args (vector->list (current-command-line-arguments)))
;; 1st arg - compiler mode
(set-compiler-mode! (first args))
;; compile the mini program using the given parsed json
(display (visualize-CFG (second args) (third args)))