#lang racket

(require html-parsing
         html-writing
         racket/format
         scribble/reader
         threading)

(define lozenge-reader (make-at-reader
                        #:command-char #\◊
                        #:syntax? #f
                        #:inside? #t))

(define (read-file-lozenge f)
  (with-input-from-file f
    (lambda () (lozenge-reader))))

(define (parse f)
  (parameterize ((current-namespace (make-base-namespace)))
    ;; Import definitions from globals.rkt if it exists
    (when (file-exists? "globals.rkt")
      (eval (quote (require "globals.rkt"))))
    ;; Read the file as text at-exp and eval each element
    (with-input-from-file f
      (lambda ()
        (~> (lozenge-reader)
            (map eval _)
            ;; Remove all void values, eg. from define
            ;;
            ;; remq for some reason only removes one, while remq* removes
            ;; all but demands the first argument to be a list.
            (remq* (list (void)) _))))))

;; Expand all Pollen expressions in f and return the string.
(define (expand f)
  (string-join (map ~a (parse f))))

;; Extract the style from a component file F.
;; (define (component-style f)
;;   (with-input-from-file f
;;     (lambda ()
;;       ;; I'd like to use assoc, but some elements aren't pairs.
;;       (findf (lambda (v) (and (cons? v) (eq? (car v) 'style)))
;;              (html->xexp (current-input-port))))))

;; Extract the markup in F (excluding <style>).
;; TODO: and turn it into a function???
(define (component-to-func f)
  (let* ((it (with-input-from-file f
               (lambda ()
                 (html->xexp (current-input-port)))))
         (it (filter
              (lambda (v)
                (and (cons? v)
                     (not (eq? (car v) 'style))))
              it))
         (it (xexp->html it)))
    (with-input-from-string it
      (lambda ()
        (lozenge-reader)))))

;; Expand the component named NAME.
;; TODO: arguments
;; TODO: we have to be able to change the namespace used by expand and parse
;; (right now they create a new one everytime they're called)
(define (component name)
  (let ((f (car (filter (lambda (path)
                          (~> path
                              (path-replace-extension "")
                              file-name-from-path
                              path->string
                              (equal? name)))
                        (directory-list "components" #:build? #t)))))
    (expand f)))
