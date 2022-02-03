#lang typed/racket

(require "compiler.rkt")

;; 1st cmd line arg: name of the .mini program filename
(define mini-name (first (vector->list (current-command-line-arguments))))
;; 2nd cmd line arg: json str representing the parsed .mini program
(define json-str (first (rest (vector->list (current-command-line-arguments)))))

;; display SUCCESS if the front-end succeeds, or the error message otherwise
(with-handlers ([exn:fail? (lambda ([exn : exn])
                             (println (string-append "EXCEPTION THROWN   " mini-name))
                             (println (string-append "                   " (exn-message exn))))])
  (define program (front-end-json json-str))
  (println (string-append "SUCCESS            " mini-name))
  (println program))