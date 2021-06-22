;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'buttercup)

;; Set to t here to show debug messages.
(defvar kpc-debug nil)

(require 'kpc)

(describe "datums"
  (it "passes"
    (expect (kpc-read-@-exp-datums "@abc[123 \"abc\"]" 4)
            :to-equal
            '((123 "abc") . 15))))

(describe "cmd"
  (it "parses"
    (expect (kpc-read-@-exp-cmd "@a[123 456 \"abc\" [vector v]]" 1)
            :to-equal '(a . 2))))

(describe "at-exp"
  (it "parses with datums"
    (expect (kpc-read-@-exp "@a[123 456 \"abc\" [vector v]]")
            :to-equal '(a 123 456 "abc" [vector v])))
  (it "parses with empty datums"
    (expect (kpc-read-@-exp "@a[]")
            :to-equal '(a)))
  (it "parses without datums"
    (expect (kpc-read-@-exp "@a")
            :to-equal 'a))
  (it "parses with just body"
    (expect (kpc-read-@-exp "@a{hello}")
            :to-equal '(a "hello")))
  (it "parses with both datums and body"
    (expect (kpc-read-@-exp "@a[:href \"kisaragi-hiu.com\"]{hello}")
            :to-equal '(a :href "kisaragi-hiu.com" "hello"))))
