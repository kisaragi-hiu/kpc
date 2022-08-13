#lang racket

(require racket/syntax-srcloc
         (only-in scribble/reader
                  (read-syntax-inside scribble/reader:read-syntax-inside)))

(provide kpc-process kpc-read/eval)

;; Read from `input-port`, using `namespace` if anything needs to
;; be evaluated.
;;
;; at-exp from the port is evaluated in `namespace` then
;; converted to a string.
;;
;; Return a list of strings.
(define (kpc-read/eval [input-port (current-input-port)]
                       [namespace (current-namespace)])
  (let ((synlist (scribble/reader:read-syntax-inside
                  (object-name input-port)
                  input-port)))
    (for/list ([syn (in-syntax synlist)])
      (let ((dat (syntax->datum syn)))
        (cond
          [(string? dat) dat]
          [(symbol? dat)
           (with-handlers ([exn:fail:contract:variable?
                            (lambda (e)
                              (format "~a" dat))])
             (format "~a" (eval dat namespace)))]
          [(list? dat)
           (with-handlers ([exn:fail:contract:variable?
                            (lambda (e)
                              (format "~a" dat))]
                           [exn:fail?
                            (lambda (e)
                              (error
                               "Failed: "
                               (struct->vector e)
                               (syntax-srcloc syn)))])
             (format "~a" (eval dat namespace)))]
          [else (format "~a" dat)])))))

;; Take `filepath`, return its content after transforming all at-exps
;; inside.
;;
;; If there is a "kpc-config.rkt" next to `filepath`, it will be
;; automatically required before code in `filepath` runs.
(define (kpc-process filepath)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-require 'racket/base)
    (when (file-exists? (build-path (path-only filepath) "kpc-config.rkt"))
      (namespace-require (build-path (path-only filepath) "kpc-config.rkt")))
    (string-join
     (with-input-from-file filepath
       (lambda ()
         (kpc-read/eval)))
     "")))
