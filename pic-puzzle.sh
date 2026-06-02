#! /bin/sh
#
# wrapper to lower GC threshold for sbcl and run lisp program
#

PROG=pic-puzzle.lisp
sbcl --dynamic-space-size 256 --load $PROG --quit
