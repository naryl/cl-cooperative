#!/bin/sh

LISP=sbcl
#LISP=ecl

$LISP --eval '(ql:quickload :cl-cooperative)' \
      --eval '(asdf:test-system :cl-cooperative)' \
      --eval '(uiop:quit)'
