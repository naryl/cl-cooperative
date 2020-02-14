#!/bin/sh

run() {
	$1 --eval '(ql:quickload :cl-cooperative)' \
		--eval '(asdf:test-system :cl-cooperative)' \
		--eval '(uiop:quit)'
	}

run sbcl
run ecl
