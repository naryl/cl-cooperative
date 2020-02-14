#!/bin/sh
sbcl --eval '(ql:quickload :cl-cooperative)' \
     --eval '(asdf:test-system :cl-cooperative)' \
     --eval '(uiop:quit)' \
     --no-linedit
