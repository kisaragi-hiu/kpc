#lang racket

(require "./kpc.rkt")

(command-line
 #:program "kpc"
 #:args (path)
 (display
  (kpc-process path)))
