;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'buttercup)

;; Set to t here to show debug messages.
(defvar kpc-debug nil)

(require 'kpc)

(describe "helpers"
  (let ((str "@a{this is not a symbol}"))
    (it "sanitizes curly braces"
      (expect (kpc--sanitize-curly-brace str)
              :to-equal "@a this is not a symbol}"))
    (it "sanitizes curly braces without mutating the argument"
      (expect str :to-equal "@a{this is not a symbol}"))))

(describe "datums"
  (it "datum reader works"
    (expect (kpc-read-@-exp-datums "@abc[123 \"abc\"]" 4)
            :to-equal
            '((123 "abc") . 15)))
  (it "allows nesting"
    (expect (kpc-read-@-exp-datums "@abc[@bar[1 2]]" 4)
            :to-equal
            '(((bar 1 2)) . 15))))

;; FIXME: not implementing comments for now
(describe "cmd"
  (it "parses with datums"
    (expect (kpc-read-@-exp-cmd "@a[123 456 \"abc\" [vector v]]" 1)
            :to-equal '(a . 2)))
  (it "parses with a body"
    (expect (kpc-read-@-exp-cmd "@a{this is not a symbol}" 1)
            :to-equal '(a . 2)))
  (it "wraps prefixes around the whole thing, not just cmd"
    ;; when parsing commands we have to extract the prefixes first
    (expect (kpc-read-@-exp "@`',@foo{blah}")
            :to-equal (quote `',@(foo "blah")))
    (expect (kpc-read-@-exp "@`(unquote foo){blah}")
            :to-equal '`(,foo "blah")))
  (it "cmd can be anything"
    (expect (kpc-read-@-exp "@a\\ b")
            :to-equal 'a\ b)
    (expect (kpc-read-@-exp "@200")
            :to-equal 200)
    (expect (kpc-read-@-exp "@\"@\"")
            :to-equal "@"))
  (it "cmd can literally be anything"
    (expect (kpc-read-@-exp "@(lambda (x) x){blah}")
            ;; Note that this calling style is deprecated in Elisp.
            ;; The proper way would be (funcall (lambda ...) ...).
            ;; I'm not sure about the support in Common Lisp.
            :to-equal '((lambda (x) x) "blah"))
    (expect (kpc-read-@-exp "@@foo{bar}{baz}")
            :to-equal '((foo "bar") "baz"))))

(describe "body"
  (it "parses bodies with nested braces"
    (expect (kpc-read-@-exp "@foo{{{}}{}}")
            :to-equal '(foo "{{}}{}"))
    (expect (kpc-read-@-exp "@foo{f{o}o}")
            :to-equal '(foo "f{o}o")))
  (it "parses bodies with simple newlines"
    ;; simple newline
    (expect (kpc-read-@-exp "@foo{blah blah\nyada yada}")
            :to-equal '(foo "blah blah" "\n" "yada yada")))
  (it "trims next line up to indentation"
    ;; trim next line up to indentation
    (expect (kpc-read-@-exp "
@foo{blah blah
     yada yada}")
            :to-equal '(foo "blah blah" "\n" "yada yada"))
    (expect (kpc-read-@-exp "
@foo{blah blah
       yada yada}")
            :to-equal '(foo "blah blah" "\n" "  " "yada yada")))
  (it "handles newlines correctly"
    ;; newlines
    (expect (kpc-read-@-exp "@foo{
  blah blah
  yada yada
}")
            :to-equal '(foo "blah blah" "\n" "yada yada")))
  (it "escaped string is merged with the surrounding text"
    (expect (kpc-read-@-exp "@foo{hello@\"@\"example.com}")
            :to-equal (foo "hello@example.com"))
    (expect (kpc-read-@-exp "@foo{A @\"}\" marks the end}")
            :to-equal (foo "A } marks the end")))
  (it "supports the |{}| syntax"
    (expect (kpc-read-@-exp "@foo|{bar}@{baz}|")
            :to-equal (foo "bar}@{baz"))
    (expect (kpc-read-@-exp "@foo|{bar |@x{X} baz}|")
            :to-equal (foo "bar " (x "X") " baz"))
    (expect (kpc-read-@-exp "@foo|{bar |@x|{@}| baz}|")
            :to-equal (foo "bar " (x "@") " baz")))
  (it "supports the punctuation nesting extension"
    (expect "@foo|--{bar}@|{baz}--|"
            :to-equal (foo "bar}@|{baz"))
    ;; "The punctuation is mirrored for parentheses and <>s."
    (expect "@foo|<<{bar}@|{baz}>>|"
            :to-equal (foo "bar}@|{baz"))
    (expect "@foo|(({bar}@|{baz}))|"
            :to-equal (foo "bar}@|{baz"))))

(describe "at-exp"
  (it "no spaces between parts"
    (expect (kpc-read-@-exp "@foo{bar @baz [2 3]}")
            :to-equal '(foo "bar " baz " [2 3]"))
    (expect (kpc-read-@-exp "@foo{bar @baz[2 3]}")
            :to-equal '(foo "bar " (baz 2 3))))
  (it "parses with just body"
    (expect (kpc-read-@-exp "@foo{blah blah blah}")
            :to-equal '(foo "blah blah blah")))
  (it "parses with just the cmd part"
    (expect (kpc-read-@-exp "@a")
            :to-equal 'a)
    (expect (kpc-read-@-exp "@(+ 1 2)")
            :to-equal '(+ 1 2)))
  (it "parses with datums and body"
    (expect (kpc-read-@-exp "@foo[1 2]{3 4}")
            :to-equal '(foo 1 2 "3 4"))
    (expect (kpc-read-@-exp "@foo[:width 2]{blah blah}")
            :to-equal '(foo :width 2 "blah blah"))
    (expect (kpc-read-@-exp "@a[:href \"kisaragi-hiu.com\"]{hello}")
            :to-equal '(a :href "kisaragi-hiu.com" "hello")))
  (it "parses with just datums"
    (expect (kpc-read-@-exp "@foo[1 2 3 4]")
            :to-equal '(foo 1 2 3 4))
    (expect (kpc-read-@-exp "@a[123 456 \"abc\" [vector v]]")
            :to-equal '(a 123 456 "abc" [vector v])))
  (it "parses with empty datums"
    (expect (kpc-read-@-exp "@a[]")
            :to-equal '(a)))
  (it "allows nesting"
    (expect (kpc-read-@-exp "@foo{bar @baz{3}\nblah}")
            :to-equal '(foo "bar " (baz "3") "\n" "blah"))
    (expect (kpc-read-@-exp "@foo{@b{@u[3] @u{4}}\nblah}")
            :to-equal '(foo (b (u 3) " " (u "4") "\n" "blah"))))
  (it "works without the cmd part"
    (expect (kpc-read-@-exp "@{blah blah}")
            :to-equal '("blah blah"))))
