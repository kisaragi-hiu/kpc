#lang racket

(require "./main.rkt")

(command-line
 #:program "kpc"
 #:args (path)
 (display
  (kpc-process path)))
