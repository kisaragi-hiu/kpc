#lang racket/base

(require html-parsing
         html-writing
         racket/list
         racket/path
         racket/port
         racket/string
         scribble/reader
         threading)

(provide (all-defined-out))

(define lozenge-reader (make-at-reader
                        #:command-char #\◊
                        #:syntax? #f
                        #:inside? #t))

(define (read-file-lozenge f)
  (with-input-from-file f
    (lambda () (lozenge-reader))))

(define (expand-sexp sexp)
  (parameterize ((current-namespace (make-base-namespace)))
    ;; Import definitions from globals.rkt if it exists
    (when (file-exists? "globals.rkt")
      (eval (quote (require "globals.rkt"))))
    ;; Eval each element
    (~> sexp
        (map eval _)
        ;; Remove all void values, eg. from define
        ;;
        ;; remq for some reason only removes one, while remq* removes
        ;; all but demands the first argument to be a list.
        (remq* (list (void)) _)
        (map (lambda (e) (format "~a" e)) _)
        string-join)))

;; Expand all Pollen expressions in f and return the string.
;;
;; Read the file with lozenge-reader and expand them.
(define (expand f)
  (with-input-from-file f
    (lambda ()
      (expand-sexp (lozenge-reader)))))

;; Like expand but excluding styles
(define (expand-exclude-styles f)
  (let ((no-styles
         (~>> (with-input-from-file f
                (lambda ()
                  (html->xexp (current-input-port))))
              (filter
               (lambda (v)
                 ;; only remove the style
                 (not (and (cons? v)
                           (eq? (car v) 'style)))))
              xexp->html)))
    (with-input-from-string no-styles
      (lambda ()
        (expand-sexp (lozenge-reader))))))

;; Extract the style from a component file F.
;; (define (component-style f)
;;   (with-input-from-file f
;;     (lambda ()
;;       ;; I'd like to use assoc, but some elements aren't pairs.
;;       (findf (lambda (v) (and (cons? v) (eq? (car v) 'style)))
;;              (html->xexp (current-input-port))))))


;; Expand the component named NAME.
;; TODO: arguments
;; TODO: we have to be able to change the namespace used by expand
;; (right now it creates a new one everytime they're called)
(define (component name)
  (let ((f (car (filter (lambda (path)
                          (~> path
                              (path-replace-extension "")
                              file-name-from-path
                              path->string
                              (equal? name)))
                        (directory-list "components" #:build? #t)))))
    (expand-exclude-styles f)))
