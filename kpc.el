;;; kpc.el --- Kisaragi's Pollen Clone -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25") (dash "2.18.0"))
;; Keywords: lisp


;; This file is not part of GNU Emacs.

;; This file is licensed under the MIT license. See LICENSE for details.

;;; Commentary:

;; I want to write this in Common Lisp, but I'll make a prototype in
;; Emacs Lisp first.

;;; Code:

(require 'dash)
(require 'cl-lib)

(eval-and-compile
  (defvar kpc-debug nil
    "Should `kpc--message' actually show anything?

Should be set before loading this file."))

(defmacro kpc--message (format-string &rest args)
  "Like `message', but with a prefix and only when `kpc-debug' is non-nil.

FORMAT-STRING: the format string to pass to `message'.
ARGS: format args used by `message'."
  (when kpc-debug
    `(message ,(concat "(kpc): " format-string) ,@args)))

(defconst kpc-given-but-empty (make-symbol "kpc-given-but-empty")
  "Unique symbol that signals a section is given but empty.")

(defun kpc--sanitize-curly-brace (str)
  "Remove the first curly brace in STR."
  ;; The string-match way:
  ;;
  ;;   (save-match-data
  ;;     (when (string-match "{" str)
  ;;       (replace-match " " nil nil str)))
  ;;
  ;; The loop way below is slower when not compiled but faster when
  ;; compiled. It also relies on features that's also available in CL.
  ;;
  ;; We have to make sure to copy the string though.
  ;;
  ;; (k/benchmark 100000
  ;;   (let ((at-exp "@a{this is not a symbol}"))
  ;;     (kpc--sanitize-curly-brace--string-match at-exp))
  ;;   (let ((at-exp "@a{this is not a symbol}"))
  ;;     (kpc--sanitize-curly-brace--loop at-exp))
  ;;   (let ((at-exp "@a{this is not a symbol}"))
  ;;     (kpc--sanitize-curly-brace--loop-pure at-exp)))
  ;;
  ;; Not compiled:
  ;; (4.278340620000039e-06
  ;;  0.00010861955130000018
  ;;  4.756440075999997e-05)
  ;;
  ;; Compiled:
  ;; (9.904285519999991e-06
  ;;  2.571869180000109e-06
  ;;  1.5654785899999883e-06)
  ;;
  ;; It's faster when I add an extra `copy-sequence'???
  (let ((str (copy-sequence str)))
    (catch 'ret
      (cl-loop with index = 0
               with length = (length str)
               until (= index length)
               do
               (when (= (aref str index) ?\{)
                 (setf (aref str index) ?\ )
                 (throw 'ret str))
               (cl-incf index)))))

(defun kpc-read-@-exp-cmd (at-exp pos)
  "Read the <cmd> of AT-EXP at POS.

Note that this reads an sexp as defined by Emacs Lisp, except
curly braces are excluded.

Return a cons: (sexp . new-pos)"
  (catch 'ret
    (when (>= pos (length at-exp))
      (throw 'ret nil))
    (when (memql (aref at-exp pos) '(?\{ ?\[))
      (throw 'ret nil))
    (let* ((curly-brace-sanitized
            (kpc--sanitize-curly-brace at-exp)))
      (read-from-string (or curly-brace-sanitized
                            at-exp)
                        pos))))

(defun kpc-read-@-exp-datums (at-exp pos)
  "Read the <datums> of AT-EXP at POS.

Each datum is either an sexp or an @-exp.

Return a cons: ((sexp ...) . new-pos)"
  (catch 'ret
    (when (>= pos (length at-exp))
      (throw 'ret nil))
    (unless (eql (aref at-exp pos) ?\[)
      (throw 'ret nil))
    (cl-incf pos)
    (cl-loop with sexps
             until (eql (aref at-exp pos) ?\])
             do (-let (((sexp . newpos)
                        (read-from-string at-exp pos)))
                  (setq pos newpos)
                  (push sexp sexps))
             finally return
             (cons (or (nreverse sexps)
                       kpc-given-but-empty)
                   ;; like `read-from-string', the pos should be after the
                   ;; region we have just read. POS is still inside at this
                   ;; point.
                   (1+ pos)))))

(defun kpc-read-@-exp-bodies (at-exp pos)
  "Read the <bodies> of AT-EXP at POS.

<bodies> is like free text, but @-forms are still expanded.

Each line is split into an element.

TODO: indentation based on length of the command part.

Return a cons: ((body ...) . new-pos)"
  (catch 'ret
    (when (>= pos (length at-exp))
      (throw 'ret nil))
    (unless (eql (aref at-exp pos) ?\{)
      (throw 'ret nil))
    (cl-incf pos)
    ;; start at level 1 because we're already inside
    (let ((level 1)
          sexps)
      (catch 'done
        (cl-loop
         with previous = pos
         do
         (let ((this-char (aref at-exp pos)))
           (cond
            ((= this-char ?\n)
             (let ((newline-blob-start pos)
                   (newline-blob-end (let ((pos pos))
                                       (while (= (aref at-exp pos) ?\n)
                                         (cl-incf pos))
                                       pos)))
               ;; push the string up to this point
               (push (substring at-exp previous pos) sexps)
               ;; push the newline blob; substring's END is exclusive
               (push (substring at-exp
                                newline-blob-start
                                newline-blob-end)
                     sexps)
               ;; advance to after the newline blob
               (setq previous newline-blob-end
                     pos newline-blob-end)))
            ;; Handling nested braces: increase level on open bracket
            ((= this-char ?\{)
             (cl-incf level))
            ;; Handling nested braces: decrease level on open bracket
            ;; If we hit the last bracket (level=0), push the last
            ;; blob and return
            ((= this-char ?\})
             (cl-decf level)
             (when (= level 0)
               (push (substring at-exp previous pos) sexps)
               (throw 'done t))))
           (cl-incf pos))))
      (cons (or (nreverse sexps)
                kpc-given-but-empty)
            ;; like `read-from-string', the pos should be after the
            ;; region we have just read. POS is still inside at this
            ;; point.
            (1+ pos)))))

;; TODO: this (or a version of it) needs to return NEWPOS as well for
;; nested parsing.
(defun kpc-read-@-exp (at-exp)
  "Read AT-EXP into an sexp.

at-expression: https://docs.racket-lang.org/scribble/reader.html"
  ;; HACK: just do this for now.
  (setq at-exp (string-trim at-exp))
  ;; Operate on the string directly so that it's easier to port to
  ;; languages without builtin buffers
  (let ((pos 0) cmd datums bodies)
    (unless (= (aref at-exp pos) ?@)
      (error "Not an @-exp"))
    (cl-incf pos)
    (-when-let* ((ret (kpc-read-@-exp-cmd at-exp pos)))
      (-setq (cmd . pos) ret))
    (kpc--message "after cmd: %s" pos)
    (-when-let* ((ret (kpc-read-@-exp-datums at-exp pos)))
      (-setq (datums . pos) ret))
    (kpc--message "after datums: %s" pos)
    (-when-let* ((ret (kpc-read-@-exp-bodies at-exp pos)))
      (-setq (bodies . pos) ret))
    (kpc--message "after bodies: %s" pos)
    (cond
     ((and (eq datums kpc-given-but-empty)
           (not bodies))
      `(,cmd))
     ((and (not datums)
           (not bodies))
      cmd)
     ((not cmd)
      `(,@datums ,@bodies))
     (t
      `(,cmd ,@datums ,@bodies)))))

(provide 'kpc)

;;; kpc.el ends here
