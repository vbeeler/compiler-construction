#lang typed/racket

(require "compiler.rkt")

(map front-end (vector->list (current-command-line-arguments)))