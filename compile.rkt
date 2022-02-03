#lang typed/racket

(require "compiler.rkt")

(display (compile-json (first (vector->list (current-command-line-arguments)))))