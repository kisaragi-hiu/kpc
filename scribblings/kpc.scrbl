#lang scribble/manual

@(require scribble/eval @; This might be discouraged but it's still
                        @; actively used by Racket Guide, so...
          (for-label racket)
          (for-label
           (only-in scribble/reader read-syntax-inside)))

@title{kpc: @"@"-expressions as generic preprocessor}

@author["Kisaragi Hiu"]

Sometimes I just want Pollen's preprocessing mode without treating the preprocessed file like a program. It causes Racket to sprinkle @code{compiled/} directories all over the place, and forces files to be written with a #lang header.

This tool instead just calls Scribble's @code{read-syntax-inside} on file contents.

@section{Command-Line Usage}

@codeblock{
raco kpc <file> > abc
}

@section{Racket API}

@defproc[(kpc-process [filepath (or/c path-string? path-for-some-system?)]) string?]{
  Processes @code{filepath} and returns the result as a string.
}

@defproc[(kpc-read/eval [input-port input-port?] [namespace namespace?]) (listof string?)]{
  Read text mode at-exp from @code{input-port}.

  Text is passed through as-is; @"@"-expressions are evaluated within @code{namespace}.

  @interaction[#:eval (make-base-eval '(require racket/port kpc))
  (with-input-from-string "Hello @code{world}!"
    (lambda ()
      (parameterize ((current-namespace (make-base-empty-namespace)))
        (namespace-require 'racket/base)
        (namespace-require 'xml)
        (eval
         '(define (code . expr) (xexpr->string `(code () ,@expr)))
         (current-namespace))
        (kpc-read/eval))))
  ]
}
