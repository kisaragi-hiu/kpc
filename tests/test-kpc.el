;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'buttercup)

(require 'kpc)

(describe "datums"
  (it "passes"
    (expect (kpc-read-@-exp-datums "@abc[123 \"abc\"]" 4)
            :to-equal
            '((123 "abc") . 17))))

(describe "cmd"
  (it "parses"
    (expect (kpc-read-@-exp-cmd "@a[123 456 \"abc\" [vector v]]" 1)
            :to-equal '(a . 2))))

(describe "at-exp"
  (it "parses"
    (expect (kpc-read-@-exp "@a[123 456 \"abc\" [vector v]]")
            :to-equal '(a 123 456 "abc" [vector v]))
    (expect (kpc-read-@-exp "@a[]")
            :to-equal '(a))
    (expect (kpc-read-@-exp "@a")
            :to-equal 'a)))
